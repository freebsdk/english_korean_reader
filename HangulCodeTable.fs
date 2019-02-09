namespace EnglishKoreanReader
open System

module HangulCodeTable =
    let ChosungToCode syllable =
        match syllable with
        | 'ㄱ' -> 0uy
        | 'ㄲ' -> 1uy
        | 'ㄴ' -> 2uy
        | 'ㄷ' -> 3uy
        | 'ㄸ' -> 4uy
        | 'ㄹ' -> 5uy
        | 'ㅁ' -> 6uy
        | 'ㅂ' -> 7uy
        | 'ㅃ' -> 8uy
        | 'ㅅ' -> 9uy
        | 'ㅆ' -> 10uy
        | 'ㅇ' -> 11uy
        | 'ㅈ' -> 12uy
        | 'ㅉ' -> 13uy
        | 'ㅊ' -> 14uy
        | 'ㅋ' -> 15uy
        | 'ㅌ' -> 16uy
        | 'ㅍ' -> 17uy
        | 'ㅎ' -> 18uy
        | _ -> 0uy
        
    let JungsungToCode syllable =
        match syllable with
        | 'ㅏ' -> 0uy
        | 'ㅐ' -> 1uy
        | 'ㅑ' -> 2uy
        | 'ㅒ' -> 3uy
        | 'ㅓ' -> 4uy
        | 'ㅔ' -> 5uy
        | 'ㅕ' -> 6uy
        | 'ㅖ' -> 7uy
        | 'ㅗ' -> 8uy
        | 'ㅘ' -> 9uy
        | 'ㅙ' -> 10uy
        | 'ㅚ' -> 11uy
        | 'ㅛ' -> 12uy
        | 'ㅜ' -> 13uy
        | 'ㅝ' -> 14uy
        | 'ㅞ' -> 15uy
        | 'ㅟ' -> 16uy
        | 'ㅠ' -> 17uy
        | 'ㅡ' -> 18uy
        | 'ㅢ' -> 19uy
        | 'ㅣ' -> 20uy
        | _ -> 0uy

    let JongsungToCode syllable = 
        match syllable with
        | 'ㄱ' -> 1uy
        | 'ㄲ' -> 2uy
        | 'ㄳ' -> 3uy
        | 'ㄴ' -> 4uy
        | 'ㄵ' -> 5uy
        | 'ㄶ' -> 6uy
        | 'ㄷ' -> 7uy
        | 'ㄹ' -> 8uy
        | 'ㄺ' -> 9uy
        | 'ㄻ' -> 10uy
        | 'ㄼ' -> 11uy
        | 'ㄽ' -> 12uy
        | 'ㄾ' -> 13uy
        | 'ㄿ' -> 14uy
        | 'ㅀ' -> 15uy
        | 'ㅁ' -> 16uy
        | 'ㅂ' -> 17uy
        | 'ㅄ' -> 18uy
        | 'ㅅ' -> 19uy
        | 'ㅆ' -> 20uy
        | 'ㅇ' -> 21uy
        | 'ㅈ' -> 22uy
        | 'ㅊ' -> 23uy
        | 'ㅋ' -> 24uy
        | 'ㅌ' -> 25uy
        | 'ㅍ' -> 26uy
        | 'ㅎ' -> 27uy
        | _ -> 0uy





    type SyllableIndex = Chosung | Jungsung | Jongsung





    type HangulChar(pChosungSyllable : char,pJungsungSyllable : char,pJongsungSyllable : char) = 

        member val chosung = ChosungToCode pChosungSyllable with get, set
        member val jungsung = JungsungToCode pJungsungSyllable with get, set
        member val jongsung = JongsungToCode pJongsungSyllable with get, set

        member private x.combineSyallable () =
            (((int x.chosung * 21) + int x.jungsung) * 28) + int x.jongsung + 0xAC00

        member x.ToChar() =
            x.combineSyallable() |> Convert.ToChar
            




    type HangulString () =
        let mutable parsePtr : int32 = 0
        let mutable syllablePtr : SyllableIndex = Chosung
        let mutable hangulAry = Array.zeroCreate<HangulChar> 65535
        
        member x.AddSyllable (syllable:char) =
            let current = hangulAry.[parsePtr]
            let next = hangulAry.[parsePtr+1]

            match syllablePtr with
            | Chosung ->
                current.chosung <- ChosungToCode syllable
                syllablePtr <- Jungsung
            | Jungsung ->
                current.jungsung <- JungsungToCode syllable
                syllablePtr <- Jongsung
            | Jongsung ->
                current.jongsung <- JongsungToCode syllable
                syllablePtr <- Chosung