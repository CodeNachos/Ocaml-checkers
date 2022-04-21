type case = int*int*int;;
type vecteur = int * int * int;;

let rec tourner_case (m:int) ((i,j,k):case) : case =
	if m = 0 then (i,j,k)
	else if (m mod 2 = 0) then tourner_case (m-2) (j,k,i)
	else tourner_case (m-1) (-k,-i,-j)
;;

tourner_case 1 (-4,2,2);;
tourner_case 2 (-2,-2,4);;
tourner_case 1 (5,-3,-2);; (* 2,-5,3 *)
tourner_case 3 (5,-3,-2);; (* -5,3,2 *)
tourner_case 2 (5,-3,-2);; (* -3,-2,5 *)

let translate (c:case)(v:vecteur):case = 
	let (c1,c2,c3) = c and (v1,v2,v3) = v in
	(c1+ v1, c2+v2, c3+v3);;
	

let diff_case (c1:case) (c2:case):vecteur = 
	let (i1,j1,k1) = c1 and (i2,j2,k2) = c2 in
		(i2-i1,j2-j1,k2-k1);;

let sont_cases_voisines (c1:case)(c2:case):bool = 
	let (i,j,k)=c1 in match c1 with
	|c1 when c2 = (i-1,j+1,k) || c2 = (i-1,j,k+1) -> true
	|c1 when c2 = (i+1,j-1,k) || c2 = (i+1,j,k-1) -> true
	|c1 when c2 = (i,j+1,k-1) || c2 = (i,j-1,k+1) -> true
	|_ -> false;;
	

	let calcul_pivot (c1:case) (c2:case):case option = 
		let (i1,j1,k1) = c1 and (i2,j2,k2) = c2 in 
			if(c1 <> c2) &&  (i1 = i2 || j1 = j2 ||k1 = k2) then
				let (d1,d2,d3) = diff_case c1 c2 in
				let (i,j,k)=(d1/2+i1,d2/2+j1,d3/2+k1) in 
				if c2 = (i+d1/2,j+d2/2,k+d3/2) then Some (i,j,k)
				else None
			else None;;
		;;

let calcul_pivot_v2 (c1:case) (c2:case): case option =
	let (i,j,k) = (diff_case c1 c2) in
		if c1 <> c2 && 
			(i=0 || j=0 || k=0) && 
			(abs(i-1) mod 2 = 1 && abs(j-1) mod 2 = 1 && abs(k-1) mod 2 = 1) then 
			Some (translate c1 (i/2,j/2,k/2))
		else None ;;


translate (0,-2,2) (diff_case (0,2,-2) (0,-2,2)) ;;
calcul_pivot (-6,3,3) (3,-6,3);;
calcul_pivot (0,2,-2) (0,-2,2);;
calcul_pivot (3,-4,1) (-3,2,1);;
calcul_pivot (3,-4,1) (3,-4,1);;
calcul_pivot (-5,3,2) (-4,3,1);;
calcul_pivot (-6,3,3) (-4,3,1);;

(* Q10 *)
let tourner_list (l:'a list) : 'a list =
	let head,tail = 
		match l with
		| [] -> ([],[])
		| e::r -> ([e], r)
	in let rec append (x: 'a list) (t:'a list) : 'a list =
		match t with
		| [] -> x
		| e::r -> e::(append x r) 
	in append head tail ;;

let rec der_list (l: 'a list) : 'a = 
	match l with 
	| [] -> failwith "Empty list does not have a last element"
	| e::r -> if r = [] then e else der_list r ;;

(* Q11 *)
(* Recursive equations: remplir_segment(m, (i,j,k)) = 
		{ m ∈ N |
			remplir_segment(1, (i,j,k)) = 
				(i,j,k)
			remplir_segment(m, (i,j,k)) = 
				(i,j+1,i-1) U remplir_segment(n-1, (i,j+1,i-1))
		}
*)

let rec remplir_segment (m: int) ((i,j,k): case) : case list = 
	if m = 1 then [(i,j,k)] 
	else (i,j,k)::(remplir_segment (m-1) (i,j+1,k-1)) ;;


(* Q12. *)
(* Recursive equations: remplir_triangle_bas(m, (i,j,k)) = 
		{ m ∈ N | 
			{ remplir_segment(m, (i,j,k)) U remplir_triangle_bas(m-1, (i-1,j+1,k)) if m>1
				[(i,j,k)]	else 
		}
*)

let rec remplir_triangle_bas (m: int) ((i,j,k): case) : case list = 
	if m>1 then remplir_segment m (i,j,k) @ 
		remplir_triangle_bas (m-1) (i-1,j+1,k)
	else [(i,j,k)] ;;

(* Q13 *)
(* Recursive equations: remplir_triangle_haut(m, (i,j,k)) = 
		{ m ∈ N | 
			{ remplir_segment(m, (i,j,k)) U remplir_triangle_haut(m-1, (i+1,j,k-1)) if m>1
				[(i,j,k)]	else 
		}
*)
let rec remplir_triangle_haut (m: int) ((i,j,k): case) : case list = 
	if m>1 then remplir_segment m (i,j,k) @ 
		remplir_triangle_haut (m-1) (i+1,j,k-1)
	else [(i,j,k)] ;;

(* Q14. *)
let rec colorie (cl:couleur) (l: case list) : case_coloree list =
	match l with
	| []-> []
	| c::r -> (c,cl) :: colorie cl r ;;
	
(* Q15. *)
let  tourner_confif (c:configuration):configuration = let (ccol,player_list,d) = c in 
  let rec count_players (pl:couleur list):int = match pl with
    |[] -> 0
    |p::lp -> 1 + count_players lp
  and tourner_ccol (lcol:case_coloree list) (m:int):case_coloree list = match lcol with
    |[] -> []
    |el::lp ->  if m = 1 then let (case,color) = el in let new_case = tourner_case m case and new_color = 
    match color with
    |Vert -> Marron
    |Marron -> Bleu
    |Bleu -> Noir
    |Noir -> Rouge
    |Rouge -> Jaune
    |Jaune -> Vert  
  in (new_case,new_color)::tourner_ccol lp m else if m = 2 then let (case,color) = el in let new_case = tourner_case m case 
  and new_color = match color with
    |Vert -> Rouge
    |Rouge -> Jaune
    |Jaune -> Vert
  in (new_case,new_color)::tourner_ccol lp m else if m = 3 then let (case,color) = el in let new_case = tourner_case m case and
  new_color = match color with
    |Vert -> Jaune
    |Jaune -> Vert
  in (new_case,new_color)::tourner_ccol lp m else failwith "Incorrect number of players"
in let mp = count_players player_list in (tourner_ccol ccol (6/mp), tourner_list player_list,d );;

(*Q.16 - Rafael)
---*)
(*Q.17*)

let quelle_couleur (c:case)(conf:configuration):couleur = let (ccol,_,_) = conf in 
  associe c ccol Libre;;
(quelle_couleur) (-1,4,-3) c1;;

(*Q.18*)

let supprime_dans_config (conf:configuration)(c:case):configuration = 
  if (quelle_couleur c conf) = Libre then conf else let (ccol,pl,d) = conf in
    let rec search_case (casecol:case_coloree list)(c:case):case_coloree list = match casecol with
      |[] -> failwith "can't be empty"
      |(case,col)::lp -> if c = case then lp else (case,col)::search_case lp c
    in (search_case ccol c, pl,d);;


(*Q.19*)
let est_coup_valide (conf:configuration)(co:coup):bool = let (ccol,pl,d) = conf in match co with
  |Du (c1,c2) -> (sont_cases_voisines c1 c2) && (quelle_couleur c1 conf)<>Libre && 
                 (quelle_couleur c2 conf ) = Libre && (est_dans_losange c2 d)
  |Sm lp -> failwith "Sauts multiples non implementés";;
(*Q.20*)
let appliquer_coup (conf:configuration)(cp:coup):configuration = match cp with
  |Du (c1,c2) -> let coul = quelle_couleur c1 conf and conf2 = supprime_dans_config conf c1 in 
    let (ccol,pl,d) = conf2 in ((c2,coul)::ccol,pl,d)
  |Sm lp -> failwith "Sauts multiples non implementés";;

(*Q.21*)
let mettre_a_jour_conf (conf:configuration) (cp:coup):configuration = 
  if est_coup_valide conf cp then appliquer_coup conf cp else failwith "Bro you did something wrong";; 
  
 (* Q.22 *)
 let est_libre_seg (c1:case)(c2:case)(conf:configuration):bool = let (ci,cj,ck) = c1 in let ((iv,jv,kv),d) = vec_et_dist c1 c2 in
  let rec est_libre_case (cs:case)(dis:int):bool = let (i,j,k) = cs in if dis = 0 then true else (quelle_couleur cs conf) = Libre &&
  est_libre_case (i+iv,j+jv,k+kv)(dis-1)
  in est_libre_case (ci+iv,cj+jv,ck+kv)(d-1);;   

(*Q.23*)

let est_saut (c1:case)(c2:case)(conf:configuration):bool = let (_,_,dim) = conf and (_,dist) = vec_et_dist c1 c2 in
  (est_dans_losange c2 dim) && (dist = 2) && est_libre_seg c1 c2 conf;;

(*Q.24*)

let rec est_saut_multiple (c:case list)(conf:configuration):bool = match c with
  |[]-> failwith "Can't operate on an empty list"
  |a::[] -> true
  |a::(b::lp) -> est_saut a b conf && est_saut_multiple (b::lp) conf;; 
