namespace Fata

module Diamond =
    let keepLetterOrSpace ls lt = 
        ls |> List.map (fun letter -> if letter = lt then letter else ' ')

    let listToString delimiter =
        List.map string 
        >> Seq.ofList 
        >> String.concat delimiter

    let diamond (letter: char) =                        
        let horizontal = [ (int letter) .. -1 .. (int 'A')] @ [ (int 'B') .. (int letter) ] |> List.map char
        
        [ (int 'A') .. (int letter)] @ [(int letter) - 1 .. -1 .. (int 'A')]
        |> List.map char
        |> List.map (keepLetterOrSpace horizontal >> listToString "")
        |> Seq.ofList 
        |> String.concat System.Environment.NewLine