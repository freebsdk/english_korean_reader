// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.ComponentModel
open EnglishKoreanReader
open EnglishKoreanReader.HangulCodeTable



[<EntryPoint>]
let main argv =
    let x = HangulString()
    x.AddSyllable 'ㅍ'
    x.AddSyllable 'ㅏ'
    x.AddSyllable 'ㄹ'
    x.AddSyllable 'ㅏ'
    x.AddSyllable 'ㄴ'
    x.AddSyllable 'ㅅ'
    x.AddSyllable 'ㅐ'
    x.AddSyllable 'ㄱ'

    printfn "%s" (x.ToString())
    0 // return an integer exit code
