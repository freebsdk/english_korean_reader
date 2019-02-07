// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.ComponentModel


type SyllableIndex = Chosung | Jungsung | Jongsung




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


    


type HangulChar () = 
    member val chosung = 0uy with get, set
    member val jungsung  = 0uy with get, set
    member val jongsung = 0uy with get, set




type HangulString () =
    let mutable parsePtr : int32 = 0
    let mutable syllablePtr : SyllableIndex = Chosung
    let mutable hangulAry = Array.zeroCreate<HangulChar> 65535
    
    member x.AddSyllable (syllable:byte) =
        let current = hangulAry.[parsePtr]
        let next = hangulAry.[parsePtr+1]

        match syllablePtr with
        | Chosung ->
            current.chosung <- syllable
            syllablePtr <- Jungsung
        | Jungsung ->
            current.jungsung <- syllable
            syllablePtr <- Jongsung
        | Jongsung ->
            current.jongsung <- syllable
            syllablePtr <- Chosung
       

let rec ParseArray ary = 
    let mutable p = 0
    let c1 = "" + ary.[p]
    let c2 = "" + ary.[p] + ary.[p+1]

    match c1 with
    | "b" -> "ㅂ"


let readPhoneticString str = 
    Seq.toList str 
    |> ParseArray
    |> ignore
    ""


[<EntryPoint>]
let main argv =
    readPhoneticString "siŋ"
    0 // return an integer exit code
