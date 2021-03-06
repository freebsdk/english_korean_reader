namespace EnglishKoreanReader
open System
open System.Text


exception BreakException

module HangulCodeTable =
    let ChosungToCode syllable =
        match syllable with
        | Some 'ㄱ' -> Some 0
        | Some 'ㄲ' -> Some 1
        | Some 'ㄴ' -> Some 2
        | Some 'ㄷ' -> Some 3
        | Some 'ㄸ' -> Some 4
        | Some 'ㄹ' -> Some 5
        | Some 'ㅁ' -> Some 6
        | Some 'ㅂ' -> Some 7
        | Some 'ㅃ' -> Some 8
        | Some 'ㅅ' -> Some 9
        | Some 'ㅆ' -> Some 10
        | Some 'ㅇ' -> Some 11
        | Some 'ㅈ' -> Some 12
        | Some 'ㅉ' -> Some 13
        | Some 'ㅊ' -> Some 14
        | Some 'ㅋ' -> Some 15
        | Some 'ㅌ' -> Some 16
        | Some 'ㅍ' -> Some 17
        | Some 'ㅎ' -> Some 18
        | _ -> None
        
    let JungsungToCode syllable =
        match syllable with
        | Some 'ㅏ' -> Some 0
        | Some 'ㅐ' -> Some 1
        | Some 'ㅑ' -> Some 2
        | Some 'ㅒ' -> Some 3
        | Some 'ㅓ' -> Some 4
        | Some 'ㅔ' -> Some 5
        | Some 'ㅕ' -> Some 6
        | Some 'ㅖ' -> Some 7
        | Some 'ㅗ' -> Some 8
        | Some 'ㅘ' -> Some 9
        | Some 'ㅙ' -> Some 10
        | Some 'ㅚ' -> Some 11
        | Some 'ㅛ' -> Some 12
        | Some 'ㅜ' -> Some 13
        | Some 'ㅝ' -> Some 14
        | Some 'ㅞ' -> Some 15
        | Some 'ㅟ' -> Some 16
        | Some 'ㅠ' -> Some 17
        | Some 'ㅡ' -> Some 18
        | Some 'ㅢ' -> Some 19
        | Some 'ㅣ' -> Some 20
        | _ -> None

    let JongsungToCode syllable = 
        match syllable with
        | Some 'ㄱ' -> Some 1
        | Some 'ㄲ' -> Some 2
        | Some 'ㄳ' -> Some 3
        | Some 'ㄴ' -> Some 4
        | Some 'ㄵ' -> Some 5
        | Some 'ㄶ' -> Some 6
        | Some 'ㄷ' -> Some 7
        | Some 'ㄹ' -> Some 8
        | Some 'ㄺ' -> Some 9
        | Some 'ㄻ' -> Some 10
        | Some 'ㄼ' -> Some 11
        | Some 'ㄽ' -> Some 12
        | Some 'ㄾ' -> Some 13
        | Some 'ㄿ' -> Some 14
        | Some 'ㅀ' -> Some 15
        | Some 'ㅁ' -> Some 16
        | Some 'ㅂ' -> Some 17
        | Some 'ㅄ' -> Some 18
        | Some 'ㅅ' -> Some 19
        | Some 'ㅆ' -> Some 20
        | Some 'ㅇ' -> Some 21
        | Some 'ㅈ' -> Some 22
        | Some 'ㅊ' -> Some 23
        | Some 'ㅋ' -> Some 24
        | Some 'ㅌ' -> Some 25
        | Some 'ㅍ' -> Some 26
        | Some 'ㅎ' -> Some 27
        | _ -> None




    let isChosungConsonants syllable = 
        match ChosungToCode syllable with
        | Some v -> true
        | _ -> false





    let isConsonants syllable =
        match JongsungToCode syllable with
        | Some v -> true
        | _ -> false




    let isVowels syllable =
        match JungsungToCode syllable with
        | Some v -> true
        | _ -> false



    // Input states for FSM
    type SyllableState = Chosung | Jungsung | Jongsung





    type HangulChar() = 

        member val chosung = None with get, set
        member val jungsung = None with get, set
        member val jongsung = None with get, set

        member private x.combineSyallable () =
            let chosungVal = match ChosungToCode x.chosung with
                                | Some v -> v
                                | _ -> 0
            let jungsungVal = match JungsungToCode x.jungsung with
                                | Some v -> v
                                | _ -> 0
            let jongsungVal = match JongsungToCode x.jongsung with
                                | Some v -> v
                                | _ -> 0

            (((chosungVal * 21) + jungsungVal) * 28) + jongsungVal + 0xAC00

        member x.ToChar() =
            x.combineSyallable() |> Convert.ToChar
            
        member x.isEmpty() =
            (x.chosung = None && x.jungsung = None && x.jongsung = None)



    type HangulString () =
        let mutable parsePtr : int32 = 0
        let mutable syllableState : SyllableState = Chosung
        let mutable hangulAry = Array.zeroCreate<HangulChar> 65535 
        
        do
            for i in 0 .. hangulAry.Length-1 do
                hangulAry.[i] <- HangulChar()






        member private x.getBeforeHanChar parsePtr =
            if parsePtr < 1 
            then None
            else Some hangulAry.[parsePtr-1]




        member private x.moveNextAndInit = 
            parsePtr <- parsePtr + 1
            syllableState <- Chosung




        member private x.onAddChosung syllable =
            let before = x.getBeforeHanChar parsePtr
            let current = hangulAry.[parsePtr]

            if isChosungConsonants syllable then
                current.chosung <- syllable
                syllableState <- Jungsung
            else if isVowels syllable then
                match before with
                | Some beforeValue ->
                    match beforeValue.jongsung with
                    | None ->
                        // When there is no more consonant to bring
                        current.jungsung <- syllable
                        x.moveNextAndInit
                    | _ ->
                        // Bring a consonant from the privious letter
                        current.chosung <- beforeValue.jongsung
                        beforeValue.jongsung <- None
                        current.jungsung <- syllable
                        syllableState <- Jongsung
                | _ ->
                    current.jungsung <- syllable
                    x.moveNextAndInit
            else
                // When add a Jongsung consonant as Chosung
                current.jongsung <- syllable
                x.moveNextAndInit






        member private x.onAddJungsung syllable =
            let current = hangulAry.[parsePtr]

            if isVowels syllable then
                current.jungsung <- syllable
                syllableState <- Jongsung
            else if isChosungConsonants syllable then
                x.moveNextAndInit
                x.onAddChosung syllable
            else
                // When add a Jongsung consonant as Jungsung
                x.moveNextAndInit
                x.onAddJongsung syllable




        member private x.onAddJongsung syllable = 
            let current = hangulAry.[parsePtr]

            if isConsonants syllable then
                current.jongsung <- syllable
            else if isVowels syllable then
                current.jungsung <- syllable

            x.moveNextAndInit





        member x.AddSyllable (syllable: char) =
            match syllableState with
            | Chosung -> x.onAddChosung (Some syllable)
            | Jungsung -> x.onAddJungsung (Some syllable)
            | Jongsung -> x.onAddJongsung (Some syllable)



        
        
        override x.ToString() =
            let mutable sb = StringBuilder()
            try            
                for i = 0 to (hangulAry.Length-1) do
                    if hangulAry.[i].isEmpty() = true then
                        raise BreakException
                    sb.Insert(i, hangulAry.[i].ToChar()) |> ignore
            with  
                BreakException -> ()
            sb.ToString()
            
