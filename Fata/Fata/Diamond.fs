namespace Fata

module Diamond =
    let keepLetterOrReplaceWithSpace ls lt = 
        ls |> List.map (fun letter -> if letter = lt then letter else (int ' '))

    let listToString delimiter =
        List.map char
        >> List.map string 
        >> Seq.ofList 
        >> String.concat delimiter

    let diamond (letter: char) =                        
        let horizontal = [ (int letter) .. -1 .. (int 'A')] @ [ (int 'B') .. (int letter) ]
        
        [ (int 'A') .. (int letter)] @ [(int letter) - 1 .. -1 .. (int 'A')]
        |> List.map (keepLetterOrReplaceWithSpace horizontal >> listToString "")
        |> Seq.ofList 
        |> String.concat System.Environment.NewLine