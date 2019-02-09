// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.ComponentModel
open EnglishKoreanReader




[<EntryPoint>]
let main argv =
    let hs = HangulCodeTable.HangulChar('ㅋ','ㅓ','ㄴ')
    printfn "%c" (hs.ToChar())
    0 // return an integer exit code
