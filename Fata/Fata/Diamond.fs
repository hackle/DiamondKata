namespace Fata

module Dimaond =
    let diamond (letter: char) = 
        let vertical = 
            [ (int 'A') .. (int letter)] @ [(int letter) - 1 .. -1 .. (int 'A')]
            |> List.map char
            |> List.map string
            |> System.String.Concat
        vertical