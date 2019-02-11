module Tests

open System
open Xunit
open EnglishKoreanReader
open HangulCodeTable

[<Fact>]
let ``Corrent hangul combind`` () =
    let hstr = HangulString()
    hstr.AddSyllable 'ㄱ'
    hstr.AddSyllable 'ㅏ'
    hstr.AddSyllable 'ㄴ'
    hstr.AddSyllable 'ㄷ'
    hstr.AddSyllable 'ㅏ'
    hstr.AddSyllable 'ㄴ'
    Assert.True(hstr.ToString() = "간단")
