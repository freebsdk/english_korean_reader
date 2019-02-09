// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.ComponentModel
open EnglishKoreanReader




[<EntryPoint>]
let main argv =
    let hs = HangulCodeTable.HangulString()
    hs.AddSyllable('ㄱ')
    hs.AddSyllable('ㅏ')
    hs.AddSyllable('ㄴ')
    hs.AddSyllable('ㄷ')
    hs.AddSyllable('ㅏ')
    hs.AddSyllable('ㄴ')
    printfn "%s" (hs.ToString())
    0 // return an integer exit code
