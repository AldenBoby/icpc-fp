module ICPC
open System

let commaSprinkler input =
    let senlist = input.ToString().Split(' ')
    let inputlist = Seq.toList senlist
    let firstelement = false  
    
    let allowedChars (string:string) = 
        let charlist = (Seq.toList (string))
        let rec checkChar (charlist:char list) = 
            match charlist with
            |h::t -> match h|>int >=97 && h|>int <= 122 || h = ' ' || h = ',' || h = '.' with
                     | true -> checkChar t
                     | false -> None
            |[] -> Some true
        checkChar charlist
    
    let rec twoChars (input:string list) =
        match input with 
        |h::t -> match h.Length >=2 with
                 |true -> match allowedChars h with 
                          | Some true -> twoChars t
                          | _ -> None
                 |false -> None
        |[] -> Some true

    let lastStop (input:string list) =
        let lastWord = List.last input
        match lastWord.EndsWith(".") with 
        | true -> Some true
        | false -> None 
    
    let firstWord (input:string list) = 
        match input.Head = "," || input.Head = "." || input.Head = "" with
        | true -> None
        | false -> Some false
    
    let rec inBetween (input:string list) = 
        match input with
        | h::t -> match h = "" || h = "." || h = "," || h.Contains("..") || h.Contains(",") && not (h.EndsWith(",")) with
                  | true -> None 
                  | false -> inBetween t 
        | [] -> Some true

    let inputValidation = 
        match twoChars inputlist with
        | Some true -> match lastStop inputlist with
                       | Some true -> match firstWord inputlist with 
                                      | Some false -> inBetween inputlist
                                      | _ -> None
                       | _ -> None 
        | _ -> None

    let checkcomma sentence = 
            let tocheck = Seq.toList sentence
            match (List.contains(',') tocheck) with 
            | true -> true
            | false -> false

    let stripcomma (word:string) =
        word.Trim(',')

    let insertcomma (word:string) =
        String.concat "" [word;","]

    let rec finder wordlist =
        match wordlist with 
        | [] -> input
        | h::t -> match (checkcomma h) with 
                    | true -> let commaword = h.ToString()
                              commaword
                    | false -> finder t
  
    let rec addcomma (nocomma:string) (comma:string) wordlist newlist = 
            match wordlist with 
            | [] -> List.rev newlist
            | h::t -> let replacee = h.ToString()
                      match (replacee.Contains(".") || replacee.Contains(",")) with
                      | true -> let newlist = h::newlist
                                addcomma nocomma comma t newlist
                      | false -> match h = replacee with 
                                 | true -> let newlist = replacee.Replace(nocomma,comma)::newlist
                                           addcomma nocomma comma t newlist
                                 | false -> let newlist = h::newlist
                                            addcomma nocomma comma t newlist

    let rec precomma wordlist (templist:string list) foundwords =
            match wordlist with
            | h::t -> match h.ToString().Contains(",") with
                      | true -> let precword = templist.Head.Trim('.')
                                match List.contains(precword) foundwords with
                                |true -> let templist = h::templist
                                         precomma t templist foundwords  
                                |false -> precword          
                      | false -> let templist = h::templist
                                 precomma t templist foundwords        
            | [] -> "List done" // no preceeding coma 

    let rec ``Find preceeding word to sprinkle`` pword wordlist (templist:string list) element  = 
            match wordlist with 
            | h::t ->  match element with
                       | false -> let templist = h::templist
                                  let firstelement = true
                                  ``Find preceeding word to sprinkle`` pword t templist firstelement
                       | true -> let listead = templist.Head.Remove(templist.Head.IndexOf('.'))
                                 match listead = pword with
                                 |true -> h
                                 |false -> match h.Trim(',','.') = pword.Trim(',','.') with
                                           |true -> match t.Head.Contains(".") || t.Head.Contains(",") with
                                                    | true -> ``Find preceeding word to sprinkle`` pword t templist element
                                                    | false -> t.Head
                                           |false -> match t.IsEmpty with
                                                     |true -> "List Empty" 
                                                     |false -> ``Find preceeding word to sprinkle`` pword t templist element
            | _ -> failwith "examine match case this should not be reachable"

    let rec runit nocommaword commaword inputlist emptylist foundwords =
        let newlist = addcomma nocommaword commaword inputlist []
        let pword = precomma (List.rev newlist) [] foundwords
        match pword with 
        |"List done" -> Some (String.concat " " (newlist))
        |_-> let foundwords = pword::foundwords
             let preceedingword = ``Find preceeding word to sprinkle`` pword (List.rev newlist) [] firstelement
             match preceedingword with
             | "List Empty" -> Some (String.concat " " (newlist))
             | _ -> let preceedingwordcomma = preceedingword|>insertcomma
                    runit preceedingword preceedingwordcomma newlist [] foundwords
    
    let foundwords = []
    let commaword = finder inputlist
    let nocommaword = stripcomma commaword

    match inputValidation with 
    |Some true ->  runit nocommaword commaword inputlist [] foundwords
    |_ -> None
   

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
