module NanoML.Utils.List

let assoc k l =
    match List.tryFind (fst >> (=) k) l with
    | Some v -> Some(snd v)
    | _ -> None

let removeAssoc k l =
    let rec rem l =
        match l with
        | h :: t when fst h = k -> t
        | h :: t -> h :: rem t
        | _ -> []
    rem l
