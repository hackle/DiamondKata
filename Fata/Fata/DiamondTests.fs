module FataTests

open Fata.Dimaond
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData('A', "A")>]
let ``For 'A' the diamond is just 'A'`` (letter: char) (expected: string) = 
    let actual = diamond letter
    Assert.Equal(actual, expected)
     
[<Theory>]
[<InlineData('B', "ABA")>]
let ``For 'B' I have ABA vertically`` (letter: char) (expected: string) = 
    let actual = diamond letter
    Assert.Equal(actual, expected)