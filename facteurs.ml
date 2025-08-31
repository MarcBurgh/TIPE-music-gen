

type automate =
  { mutable taille : int;
    initial : int;
    mutable transitions : (string * int) list array;
    mutable final : bool array };;



let print_list_tuple lst =
  List.iter (fun (s, i) -> Printf.printf "(%s, %d) " s i) lst;
  print_newline ();;

let print_musique lst =
  List.iter (fun s -> Printf.printf "%s " s ) lst;
  print_newline ();;

type musique = string list 


(***********************************  Fonction SP - naif  ******************************************)
(* Fonction pour construire l'automate de base à partir d'une musique *)
let construire_automate_base (musique : string list) : automate =
  let n = List.length musique in
  (* Initialisation de l'automate *)
  let auto = {
    taille = n + 1;
    initial = 0;
    transitions = Array.make (n + 1) [];
    final = Array.make (n + 1) true
  } in
  (* Création des transitions entre les états *)
  let rec construire_etats i = function
    | [] -> ()
    | x :: xs ->
        auto.transitions.(i) <- (x, i + 1) :: auto.transitions.(i);
        construire_etats (i + 1) xs
  in
  construire_etats 0 musique;
  auto
;;
(*Complexité : O(n)*)

let prefixes_jusqua_i (mot : string list) (i : int) : (string list array) =
  let tab = Array.make (i+1) [] in 
  let rec aux lst1 lst2 compt = match compt with 
  | compt when compt=(i+1) -> tab 
  | compt -> match lst2  with 
    | [] -> tab.(compt) <- lst1; tab
    | x::xs -> begin
    tab.(compt) <- lst1 ;
    aux (lst1@[x]) xs (compt+1);
  end
  in aux [] mot 0  
;;
(* Complextié O(i) *)


let suffixes_i (tab_pref : string list array) (i : int): string list array = 
  let tab = Array.make ((i*(i+1)/2)+1) [] in let compt= ref 1 in tab.(0) <- [] ;
  for a = 1 to i do 
    let lst_act = List.rev tab_pref.(a) in 
    let tab_temp = prefixes_jusqua_i lst_act (List.length lst_act ) in 
    for j=1 to Array.length tab_temp -1 do begin 
      tab.(!compt) <- List.rev tab_temp.(j);
      incr compt end 
    done;
  done;
  tab
;;
(* Complexité : O(i²)*)

(*On se rend compte que le tableau contient tous les facteurs du mot considéré (cad le mot restreint de 0 à i)*)

let plus_long_suffixe_double arr =
  let open List in
  (* Transforme le tableau en une liste pour faciliter le traitement *)
  let lst = Array.to_list arr in
  (* Compte les occurrences de chaque élément dans la liste *)
  let counts = fold_left (fun acc x ->
      let count = try List.assoc x acc with Not_found -> 0 in
      (x, count + 1) :: (remove_assoc x acc)
    ) [] lst
  in
  (* Filtrer les listes qui apparaissent au moins deux fois *)
  let duplicates = filter (fun (_, count) -> count >= 2) counts in
  (* Trouver la liste de taille maximale parmi les doublons *)
  match duplicates with
  | [] -> None  (* Aucun doublon trouvé *)
  | _ ->
      let max_list = fst (fold_left (fun (max_list, max_len) (lst, _) ->
          let len = List.length lst in
          if len > max_len then (lst, len) else (max_list, max_len)
        ) ([], 0) duplicates)
      in
      Some max_list
    ;;
  ;;
;;
(*Complexité : O(taille du tableau au carré) = O(i⁴)*)

(* Il ne reste donc plus qu'une étape avant de pouvoir construire notre fonction Sp : il faut maintenant, 
si on a bien un plus grand facteur doublon non vide, pouvoir déterminer l'indice de la fin de la première occurence du mot. 
*)


let recherche mot motif = (* On part du principe qu'on utilisera cette fonction si l'on sait deja qu'il existe une occurence*)
  let rec aux mot motif motif_act position= match motif_act with 
  | [] -> position 
  | x::xs -> match mot with 
  | [] -> 0 (*Cas inutilisé en pratique*)
  | b::bs -> if b=x then aux bs motif xs (position+1) else aux bs motif motif (position+1)
in aux mot motif motif 0;;
(*complexité : O(n*m) = O(n²) pire des cas mais très peu probable*)


let sp mot i = 
  if i=0 then -1 
  else match plus_long_suffixe_double (suffixes_i (prefixes_jusqua_i mot i ) i) with 
  | None -> 0
  | Some lst -> recherche mot lst 
;;
(* Complexité O(i⁴ + n² )*)



(***************************** ORACLE DES FACTEURS **********************************************)



let non_transition_par_sigma_depuis_k sigma k auto = (* Verifie qu'il n'existe pas de transition par sigma depuis l'état k*)
  let lst = auto.transitions.(k) in 
  let rec aux liste = match liste with 
    | [] -> true 
    | (a,b)::xs -> if a=sigma then false else aux xs 
  in aux lst 
;;
(* O(|Sigma|)*)

let add_letter (auto, sp) sigma =
  let m = auto.taille - 1 in
  let new_size = m + 2 in

  (* Redimensionner l'automate si besoin *)
  let resize_array arr default =
    let old_len = Array.length arr in
    if old_len >= new_size then arr
    else
      let new_arr = Array.init new_size (fun i ->
          if i < old_len then arr.(i) else default)
      in new_arr
  in 
  auto.transitions <- resize_array auto.transitions [];
  auto.final <- resize_array auto.final true;

  (* Ajouter un nouvel état *)
  auto.taille <- new_size;

  (* Ajouter une transition de m -> m+1 étiquetée par sigma *)
  auto.transitions.(m) <- (sigma, m + 1) :: auto.transitions.(m);

  (* Étendre sp si besoin *)
  let sp =
    if Array.length sp >= new_size then sp
    else
      let new_sp = Array.make new_size (-1) in
      Array.blit sp 0 new_sp 0 (Array.length sp);
      new_sp
  in

  
  (* Boucle pour créer transitions manquantes *)
  let k = ref sp.(m) in 
  while !k> -1 && non_transition_par_sigma_depuis_k sigma !k auto do 
    auto.transitions.(!k) <- (sigma, m + 1) :: auto.transitions.(!k);
    k := sp.(!k)
  done;

  (* Déterminer le suffix link de m+1 *)
  let s = 
    if !k = -1 then 0
    else List.assoc sigma auto.transitions.(!k)
  in
  sp.(m + 1) <- s;

  (auto, sp)

(* Fonction oracle_on_line *)
let oracle_on_line (mot : musique) : automate * int array =
  let auto = {
    taille = 1;
    initial = 0;
    transitions = [| [] |];
    final = [| true |]
  } in
  let sp = Array.make 1 (-1) in
  List.fold_left (fun (a, sp) sigma -> add_letter (a, sp) sigma) (auto, sp) mot
    
    

let reconstruction mot alpha n = 
  let taille = ref 0 in 
  let auto,sp = oracle_on_line mot in 
  let etat_actuel = ref 0 in
  let resultat = ref [] in 
  let q = ref alpha in 
  while !taille <n do 
    let s = sp.(!etat_actuel) in 
    if s =0 || s= -1 then q := 1.
    else q := alpha;
    let lst = auto.transitions.(!etat_actuel) in 
    let p = Random.float 1. in 
    match p with 
    | p when p < !q -> 
        if lst = [] then etat_actuel := 0 
        else 
          begin
            incr etat_actuel;
            resultat := (List.nth mot !etat_actuel)::!resultat;
            incr taille
          end
    | _ -> 
        if lst = [] then etat_actuel := 0 
        else 
          let alea = Random.full_int (List.length lst) in (*On choisit aléatoirement une transition*)
          let transitio = List.nth lst alea in
          resultat := (fst transitio)::!resultat ;
          etat_actuel := snd transitio;
          incr taille
  done;
  List.rev !resultat
    
(*
let p0 = ["sol";"re";"si";"la";"si";"re";"si";"re";"sol"];;

let auto,tab = oracle_on_line p0;;
let resultat = reconstruction p0 0.5 10 ;;
*)


(***************************** IMPORTATION/EXPORTATION EN FICHIER *********************************)

let append_words_to_file filename words =
  (* Ouvre le fichier en mode ajout *)
  let oc = open_out_gen [Open_append; Open_creat] 0o666 filename in
  try
    (* Convertit la liste de mots en une seule chaîne avec des espaces *)
    let content = String.concat " " words in
    (* Écrit la chaîne dans le fichier avec un espace ou un saut de ligne si nécessaire *)
    output_string oc (content ^ " ");
    (* Ferme le fichier après l'écriture *)
    close_out oc
  with e ->
    close_out oc; (* Assure la fermeture du fichier en cas d'erreur *)
    raise e;;

let reconstruction_live mot n fichier = (* On génère une musique en utilisation la reconstruction n fois *)
  let t = List.length mot in 
  let rec aux musique_act compt = match compt with 
  | compt when compt = n -> ()
  | compt -> begin 
    print_musique musique_act;
    append_words_to_file fichier musique_act;
    aux (reconstruction_alea_taille musique_act t) (compt+1) end
  in aux mot 0
;;

let musique_pb = ["sol";"re";"si";"re";"si";"re";"sol";"sol";"re"];;
let musique = ["sol"; "re"; "si"; "la"; "si"; "re"; "si"; "re"; "sol"];;

let afficher_automate auto =
  Printf.printf "Automate de taille : %d\n" auto.taille;
  for i = 0 to Array.length auto.transitions - 1 do
    List.iter (fun (note, suivant) ->
      Printf.printf "État %d --[%s]--> État %d\n" i note suivant
    ) auto.transitions.(i)
  done;
  Printf.printf "États finaux :\n";
  Array.iteri (fun i est_final ->
    if est_final then Printf.printf "État %d est final\n" i
  ) auto.final
;;

let read_words_from_file filename =
  (* Ouvre le fichier en mode lecture *)
  let ic = open_in filename in
  try
    (* Lire tout le contenu du fichier et le découper en mots *)
    let words = 
      let rec read_words acc =
        try
          (* Lit un mot à la fois, séparé par des espaces *)
          let word = input_line ic in
          read_words (String.split_on_char ' ' word @ acc)
        with End_of_file -> List.rev acc
      in
      read_words [] 
    in
    (* Ferme le fichier après la lecture *)
    close_in ic;
    words
  with e ->
    close_in ic; (* Assure la fermeture du fichier en cas d'erreur *)
    raise e;;
  ;;
;;



(*****************************  PROBLEME MEDIAN STRING ***************************)


let min_triple a b c = 
  min a (min b c)

let rec distance_levenshtein_naif (a:string list) (b:string list) : int = match a,b with 
  | [],_ | _,[] -> max (List.length a) (List.length b)
  | x::xs, y::ys when x=y -> distance_levenshtein_naif xs ys 
  | x::xs , y::ys -> 1 + min_triple (distance_levenshtein_naif(y::ys) xs) (distance_levenshtein_naif (x::xs) ys) (distance_levenshtein_naif xs ys)

(*Complexité en O(3^n), très importante, donc on va privilégier une implémentation en programmation dynamique*)

let distance_levenshtein (a: string list) (b: string list) : int =
  let m = List.length a in
  let n = List.length b in
  let a_arr = Array.of_list a in
  let b_arr = Array.of_list b in
  let d = Array.make_matrix (m+1) (n+1) 0 in
  
  (* Initialisation de la première ligne et première colonne *)
  for i = 0 to m do
    d.(i).(0) <- i
  done;
  for j = 0 to n do
    d.(0).(j) <- j
  done;

  (* Remplissage de la matrice *)
  for i = 1 to m do
    for j = 1 to n do
      let cost = if a_arr.(i-1) = b_arr.(j-1) then 0 else 1 in
      d.(i).(j) <- min (d.(i-1).(j) + 1)       (* Suppression *)
                     (min (d.(i).(j-1) + 1)   (* Insertion *)
                          (d.(i-1).(j-1) + cost)) (* Substitution *)
    done;
  done;

  d.(m).(n)


type echantillon = string list list 
    
let rec distance_globale echantillon musique = match echantillon with
  | [] -> 0
  | x::xs -> distance_levenshtein musique x + distance_globale xs musique



         
let alpha = 0.2;;
(*let mot_genere = reconstruction musique_mozart alpha 57  
               
 let k = distance_globale echantillon_mozart mot_genere   -   environ 220 *)
  

let minimum_naif (alphabet : string list) (taille : int) =
  let min_ref = ref max_int in
  let rec aux acc n =
    if n = 0 then
      let res = distance_globale echantillon_mozart (List.rev acc) in
      if res < !min_ref then min_ref := res else ()
    else
      List.iter (fun lettre ->
          aux (lettre :: acc) (n - 1)
        ) alphabet
  in
  aux [] taille;
  !min_ref
(* En pratique infaisable car complexité qui explose*)


let alphabet = ["do";"re";"mi";"fa";"sol";"la";"si"]
    

let mot_aleatoire (alphabet : string list) (taille : int) : string list =
  Random.self_init;
  let n = List.length alphabet in
  let alphabet_array = Array.of_list alphabet in
  let rec gen acc k =
    if k = 0 then acc
    else
      let i = Random.int n in
      gen (alphabet_array.(i) :: acc) (k - 1)
  in
  gen [] taille
    
let moy_par_echantillonnage (* On va faire une moyenne sur des mots aléatoires pour situer notre résultat*)
    (alphabet : string list)
    (taille : int) 
    (echantillons : int) : int =
  let somme = ref 0 in
  for _ = 1 to echantillons do
    let mot = mot_aleatoire alphabet taille in
    let res = distance_globale echantillon_mozart mot in
    somme := !somme + res
  done;
  !somme /echantillons
    
(*let k = moy_par_echantillonnage alphabet 19 250*)
let echantillon_mozart : echantillon = [
  ["do"; "re"; "mi"; "fa"; "sol"; "fa"; "mi"; "re"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "sol"; "fa"; "mi"; "re"; "do";
   "sol"; "la"; "si"; "do"; "re"; "do"; "si"; "la"; "sol"; "la"; "si"; "do"; "re"; "mi"; "re"; "do"; "si"; "la"; "sol";
   "mi"; "fa"; "sol"; "la"; "si"; "la"; "sol"; "fa"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "si"; "la"; "sol"; "fa"; "mi"]
  ;
  ["do"; "mi"; "sol"; "si"; "do"; "si"; "sol"; "mi"; "do"; "mi"; "sol"; "si"; "do"; "re"; "do"; "si"; "sol"; "mi"; "do";
   "re"; "fa"; "la"; "do"; "re"; "do"; "la"; "fa"; "re"; "fa"; "la"; "do"; "re"; "mi"; "re"; "do"; "la"; "fa"; "re";
   "mi"; "sol"; "si"; "re"; "mi"; "re"; "si"; "sol"; "mi"; "sol"; "si"; "re"; "mi"; "fa"; "mi"; "re"; "si"; "sol"; "mi"]
  ;
  ["fa"; "la"; "do"; "mi"; "fa"; "mi"; "do"; "la"; "fa"; "la"; "do"; "mi"; "fa"; "sol"; "fa"; "mi"; "do"; "la"; "fa";
   "sol"; "si"; "re"; "fa"; "sol"; "fa"; "re"; "si"; "sol"; "si"; "re"; "fa"; "sol"; "la"; "sol"; "fa"; "re"; "si"; "sol";
   "la"; "do"; "mi"; "sol"; "la"; "sol"; "mi"; "do"; "la"; "do"; "mi"; "sol"; "la"; "si"; "la"; "sol"; "mi"; "do"; "la"]
  ;
  ["si"; "re"; "fa"; "la"; "si"; "la"; "fa"; "re"; "si"; "re"; "fa"; "la"; "si"; "do"; "si"; "la"; "fa"; "re"; "si";
   "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol";
   "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"]
  ;
  ["sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re";
   "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do";
   "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"]
  ;
  ["mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la";
   "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do";
   "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"]
]
  
let musique_mozart = [
  "do"; "re"; "mi"; "fa"; "sol"; "fa"; "mi"; "re"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "sol"; "fa"; "mi"; "re"; "do";
  "sol"; "la"; "si"; "do"; "re"; "do"; "si"; "la"; "sol"; "la"; "si"; "do"; "re"; "mi"; "re"; "do"; "si"; "la"; "sol";
  "mi"; "fa"; "sol"; "la"; "si"; "la"; "sol"; "fa"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "si"; "la"; "sol"; "fa"; "mi";
  "do"; "mi"; "sol"; "si"; "do"; "si"; "sol"; "mi"; "do"; "mi"; "sol"; "si"; "do"; "re"; "do"; "si"; "sol"; "mi"; "do";
  "re"; "fa"; "la"; "do"; "re"; "do"; "la"; "fa"; "re"; "fa"; "la"; "do"; "re"; "mi"; "re"; "do"; "la"; "fa"; "re";
  "mi"; "sol"; "si"; "re"; "mi"; "re"; "si"; "sol"; "mi"; "sol"; "si"; "re"; "mi"; "fa"; "mi"; "re"; "si"; "sol"; "mi";
  "fa"; "la"; "do"; "mi"; "fa"; "mi"; "do"; "la"; "fa"; "la"; "do"; "mi"; "fa"; "sol"; "fa"; "mi"; "do"; "la"; "fa";
  "sol"; "si"; "re"; "fa"; "sol"; "fa"; "re"; "si"; "sol"; "si"; "re"; "fa"; "sol"; "la"; "sol"; "fa"; "re"; "si"; "sol";
  "la"; "do"; "mi"; "sol"; "la"; "sol"; "mi"; "do"; "la"; "do"; "mi"; "sol"; "la"; "si"; "la"; "sol"; "mi"; "do"; "la";
  "si"; "re"; "fa"; "la"; "si"; "la"; "fa"; "re"; "si"; "re"; "fa"; "la"; "si"; "do"; "si"; "la"; "fa"; "re"; "si";
  "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol";
  "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa";
  "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re";
  "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do";
  "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si";
  "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la";
  "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do"; "re"; "mi"; "fa"; "sol"; "la"; "si"; "do";
  "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"; "la"; "sol"; "fa"; "mi"; "re"; "do"; "si"
]
