module Tests

open System
open Xunit
open EnglishKoreanReader
open HangulCodeTable

[<Fact>]
let ``My test`` () =
    let hstr = HangulString()
    Assert.True(true)
