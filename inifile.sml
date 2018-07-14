structure IniFile :> sig
  type ini_file

  val lookup : ini_file -> string -> string -> string option

  val fromList : (string * (string * string) list) list -> ini_file
  val toList : ini_file -> (string * (string * string) list) list

  val fromString : string -> ini_file option
  val fromFile : string -> ini_file option
end = struct
  type ini_file = (string * (string * string) list) list

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  fun lookup' (name, []) = NONE
    | lookup' (name, (name', value)::xs) =
        if name = name' then SOME value
        else lookup' (name, xs)

  fun lookup iniFile profileName entryName =
        lookup' (profileName, iniFile) >>= (fn entries =>
        lookup' (entryName, entries))

  fun fromList l = l
  fun toList i = i

  val dropSpace = Substring.dropl Char.isSpace
  val dropSpaceR = Substring.dropr Char.isSpace

  fun isEmptyLine line =
        let
          val line = dropSpace line
        in
          Substring.isEmpty line
            orelse Substring.sub (line, 0) = #"#"
            orelse Substring.sub (line, 0) = #";"
        end

  fun skipEmptyLines inputLine strm =
        case inputLine strm of
             NONE => NONE
           | SOME (line, strm') =>
               if isEmptyLine (Substring.full line) then
                 skipEmptyLines inputLine strm'
               else
                 SOME strm

  fun consumeChar c s =
        case Substring.getc s of
             NONE => NONE
           | SOME (c', s') =>
               if c  = c' then SOME (c', s')
               else NONE

  fun consumeUntil c s = Substring.splitl (fn c' => c <> c') s

  fun consumeProfileLine inputLine strm =
        inputLine strm                         >>= (fn (line, strm) =>
        SOME (dropSpace (Substring.full line)) >>= (fn line =>
        consumeChar #"[" line                  >>= (fn (_, line) =>
        SOME (consumeUntil #"]" line)          >>= (fn (name, line) =>
        consumeChar #"]" line                  >>= (fn (_, line) =>
        if isEmptyLine line then
          SOME (Substring.string name, strm)
        else
          NONE)))))

  fun consumeEntry inputLine strm =
        inputLine strm                         >>= (fn (line, strm) =>
        SOME (dropSpace (Substring.full line)) >>= (fn line =>
        SOME (consumeUntil #"=" line)          >>= (fn (name, line) =>
        consumeChar #"=" line                  >>= (fn (_, line) =>
        SOME (dropSpace line)                  >>= (fn line =>
        SOME (consumeUntil #" " line)          >>= (fn (value, line) =>
        if isEmptyLine line then
          let
            val name = Substring.string (dropSpaceR name)
            val value = Substring.string (dropSpaceR value)
          in
            SOME ((name, value), strm)
          end
        else
          NONE))))))

  fun consumeEntries inputLine strm =
        let
          fun loop strm entries =
                case skipEmptyLines inputLine strm of
                     NONE => (rev entries, strm)
                   | SOME strm =>
                       (case consumeEntry inputLine strm of
                             NONE => (rev entries, strm)
                           | SOME (entry, strm) =>
                               loop strm (entry::entries))
        in
          loop strm []
        end

  fun fromStream inputLine strm =
        let
          fun loop strm profiles =
                case skipEmptyLines inputLine strm of
                     NONE => SOME (rev profiles)
                   | SOME strm =>
                         (case consumeProfileLine inputLine strm of
                               NONE =>
                                 (case skipEmptyLines inputLine strm of
                                       NONE => SOME (rev profiles)
                                     | SOME _ => NONE)
                             | SOME (profileName, strm) =>
                                 let
                                   val (entries, strm) =
                                     consumeEntries inputLine strm
                                 in
                                   loop strm ((profileName, entries)::profiles)
                                 end)
        in
          loop strm []
        end

  fun fromString s =
        fromStream
          TextIO.StreamIO.inputLine
          (TextIO.getInstream (TextIO.openString s))

  fun fromFile fileName =
        let
          val ins = TextIO.getInstream (TextIO.openIn fileName)
        in
          fromStream TextIO.StreamIO.inputLine ins
          before TextIO.StreamIO.closeIn ins
        end
end
