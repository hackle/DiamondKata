namespace Fata

module Diamond =
    let filterRow row letterToKeep = 
        row |> List.map (fun letter -> if letter = letterToKeep then letter else (int ' '))

    let joinRow delimiter =
        List.map char
        >> List.map string 
        >> Seq.ofList 
        >> String.concat delimiter

    let diamond (letter: char) =                        
        let horizontal = [ (int letter) .. -1 .. (int 'A')] @ [ (int 'B') .. (int letter) ]
        
        [ (int 'A') .. (int letter)] @ [(int letter) - 1 .. -1 .. (int 'A')]
        |> List.map (filterRow horizontal >> joinRow "")
        |> Seq.ofList 
        |> String.concat System.Environment.NewLine