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
            | _ -> failwith "examine match case, this should not be reachable"

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
    let wordarray = input.ToString().Split(' ')
    let wordlist = Seq.toList wordarray
    let listi = Seq.toList (input)
    let backwards = false

    let onlyLetters (list:string) = 
        let charlist = Seq.toList list
        let rec checkCase (clist:char list) = 
            match clist with 
            |h::t -> match h|>int >=97 && h|>int <= 122 || h = ' ' || h|>int >=65 && h|>int <= 90 with 
                        |true -> checkCase t
                        |false -> None
            |[]-> Some true
        checkCase charlist
    
    let singleSpace (list:string list) = 
        match (List.contains("") list) with 
        | true -> None 
        | _ -> Some true

    let rec longestWord length (list:string list) =
        match list with 
        |h::t -> match h.Length > length with
                 | true -> match h.Length >80 with
                           |false -> longestWord h.Length t
                           |_-> None
                 | _-> longestWord length t
        |[] -> Some length

    let twoWords (list:string list)= 
       match List.length list >=2 with
       |true -> Some true
       |false -> None

    let validation = 
        match onlyLetters input with
        |Some true -> match singleSpace wordlist with
                      | Some true -> match longestWord 0 wordlist with
                                     | None  -> None
                                     | _ -> match twoWords wordlist with
                                            | Some true -> Some true
                                            | _ -> None
                      |_-> None
        |_-> None
    
    let findLargest accumulator x = 
        match x > accumulator with 
        |true -> x 
        |_ -> accumulator
    
    let maxim malum valum = 
        match malum with 
                  |(b,a) -> match valum with
                            | (d,c) -> match c>a with
                                       |true -> (d,c)
                                       |false -> (b,a)
    
    let rec realsplit (list:string list) (newlist:string list) biglist length ogLength =
        let og = ogLength
        match list with
        | [] -> let hello = String.concat " " (List.rev newlist)
                let biglist = hello::biglist(*List.iter (printfn "%s")*) 
                List.iter (printfn "%s") (List.rev biglist)
                (List.rev biglist)
        | h::t -> match h.Length <= length  with
                  | true -> let newlist = h::newlist
                            realsplit t newlist biglist ((length - h.Length)-1) og
                  | false -> let hello = String.concat " " (List.rev newlist)
                             let biglist = hello::biglist
                             realsplit list [] biglist og og

    let rec comparespace (list:string list) rivercount (startIndex:int) fullList (rivercountlist:int list) backwards=
        match list with 
        | [] -> rivercountlist |> List.fold findLargest 0
        | h::t -> match h.IndexOf(' ',startIndex) with 
                  | -1 -> comparespace t 0 0 t rivercountlist backwards
                  | _ -> let targetIndex = h.IndexOf(' ',startIndex)
                         let targetIndexplus =  targetIndex+1
                         let targetIndexminus = targetIndex-1
                         match t.IsEmpty with
                         |true -> match rivercount with
                                  | 0 ->  1::rivercountlist |> List.fold findLargest 0
                                  | _ ->  rivercountlist |> List.fold findLargest 0
                         |false -> 
                                   let checki = t.Head //
                                   match checki.Length> targetIndex+1 with
                                   | false ->let rivercountlist = (rivercount+1)::rivercountlist
                                             comparespace t 0 0 t rivercountlist backwards
                                   | true -> match checki.[targetIndex] = ' ' || checki.[targetIndexplus] = ' ' || checki.[targetIndexminus] = ' ' with
                                             |true -> match checki.[targetIndex] = ' ' with
                                                        | true ->   comparespace t (rivercount+1) targetIndex fullList rivercountlist backwards
                                                        | false ->  match checki.[targetIndexplus] = ' ' with
                                                                    |true -> comparespace t (rivercount+1) targetIndexplus fullList rivercountlist backwards
                                                                    | false ->let backwards = true
                                                                              comparespace t (rivercount+1) targetIndexminus fullList rivercountlist backwards
                                             |false ->let rivercountlist = (rivercount+1)::rivercountlist
                                                      match backwards with 
                                                      | true -> comparespace fullList 0 (targetIndex+(rivercount+1)) fullList rivercountlist false
                                                      | false -> comparespace fullList 0 (targetIndex+1) fullList rivercountlist false
                                                      
    
    let minLen = match longestWord 0 wordlist with
                     | None -> -1//validation function will catch this case, -1 is just to bind during build
                     | Some a -> a
    let maxLen = input.Length

    let rec locateLongestRiver iter max riverlineList = 
        let fully = (realsplit wordlist [] [] iter iter)
        let rivercountlinex = comparespace fully 0 0 fully [] backwards
        let riverlineList = (iter,rivercountlinex)::riverlineList
        match iter >= max with
        |true -> Some (List.rev riverlineList |> List.fold maxim (0,0))
        |false -> locateLongestRiver (iter+1) max riverlineList

    match validation with 
    | Some true -> locateLongestRiver minLen maxLen []
    | _ -> None

[<EntryPoint>]
let main argv =
    //printfn "Hello World from F#!"
    //printfn "%A" (rivers "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country")
    //printfn "%A" (rivers "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers")
    //printfn "%A" (rivers "hello world")
    Console.ReadKey()
    0 // return an integer exit code
