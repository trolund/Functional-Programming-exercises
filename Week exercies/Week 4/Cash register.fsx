(*
Make a revised version of the Cash register example in Section 4.6 where:
    (1) The function findArticle is replaced by an application of List.tryFind
    (2) The function makeBill is declared using List.foldBack
*)

type ArticleCode = string
type ArticleName = string
type NoPieces = int
type Price = int
type RegItem = ArticleCode * (ArticleName * Price)
type Register = Map<ArticleCode, (ArticleName * Price)>
type ListRegister = RegItem list
type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price
type Item = NoPieces * ArticleCode
type Purchase = Item list

// map and list vertions of reg.
let reg1 =
    Map.ofList [ "a1", ("herring", 12)
                 "a2", ("cheese", 25) ]

let reglist =
    [ "a1", ("herring", 12)
      "a2", ("cheese", 25) ]


let rec findArticle ac =
    function
    | (ac', adesc) :: _ when ac = ac' -> adesc
    | _ :: reg -> findArticle ac reg
    | _ -> failwith (ac + " is an unknown article code")

findArticle "a1" reglist

let rec tryFindArticle ac reg =
    match List.tryFind (fun (name, _) -> name = ac) reg with
    | None -> failwith (ac + " is an unknown article code")
    | Some ((_, (x, y))) -> (x, y)

tryFindArticle "a1" reglist

let rec makeBill reg =
    function
    | [] -> ([], 0)
    | (np, ac) :: pur ->
        let (aname, aprice) = findArticle ac reg
        let tprice = np * aprice
        let (billtl, sumtl) = makeBill reg pur
        ((np, aname, tprice) :: billtl, tprice + sumtl)


let rec makeBill1 reg =
    function
    | [] -> ([], 0)
    | (np, ac) :: pur ->
        match Map.tryFind ac reg with
        | Some (aname, aprice) ->
            let tprice = np * aprice
            let (infos, sumbill) = makeBill1 reg pur
            ((np, aname, tprice) :: infos, tprice + sumbill)
        | None -> failwith (ac + " is an unknown article code")



let pur = [ (3, "a2"); (1, "a1") ]

makeBill1 reg1 pur
makeBill reglist pur

// with a map

let makeBill2 reg pur =
    List.foldBack (fun (np, ac) (l, sum) ->
        match Map.tryFind ac reg with
        | Some (name, price) ->
            let res = np * price
            ((name, res) :: l, res + sum)
        | None -> failwith (ac + " is an unknown article code")) pur ([], 0)

makeBill2 reg1 pur

// with a list

let makeBill3 reg pur =
    List.foldBack (fun (np, ac) (l, sum) ->
        match List.tryFind (fun (n, _) -> n = ac) reg with
        | Some (n, (name, price)) ->
            let res = np * price
            ((name, res) :: l, res + sum)
        | None -> failwith (ac + " is an unknown article code")) pur ([], 0)

makeBill3 reglist pur
