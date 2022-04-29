type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

(*A REMPLIR*)
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
	((i >= -dim && i <= 2*dim) &&  
	(j >= -dim) &&
	(k >= -dim )) 
	||
	((i >= -2*dim && i <= dim) &&  
	(j <= dim) &&
	(k <= dim )) 
;;

let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
    (i >= -2 * dim && i <= 2 * dim) &&  
    (j <= dim && j >= -dim) &&
    (k <= dim && k >= -dim) 
;;
    
let rec associe a l defaut=
  match l with
  | [] -> defaut
  | (a2, b) :: suite -> if a = a2 then b else associe a suite defaut;;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(* La configuration initiale avec deux joueurs et un plateau de dimension 2*)
let conf_init : configuration =
  ([((3, -1, -2), Jaune); ((3, -2, -1), Jaune); ((4, -2, -2), Jaune);
    ((5, -3, -2), Jaune); ((-3, 1, 2), Vert); ((-3, 2, 1), Vert);
    ((-4, 2, 2), Vert); ((-5, 3, 2), Vert)],
   [Vert; Jaune], 2);;

affiche conf_init;;

(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)




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

let vec_et_dist (c1:case)(c2:case):vecteur*int =
  let trans=(diff_case c1 c2) in 
    let (t1,t2,t3)=trans in 
      if (t1=0 && t2<>0 && t3<>0) || 
				 (t1<>0 && t2=0 && t3<>0) || 
			   (t1<>0 && t2<>0 && t3=0) then
        		let max1= max (t1) (max t2 t3) in 
          	((t1/max1, t2/max1, t3/max1),max1) 
      else failwith "The vectors are not collinear";;

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

(* Auxiliary functions for Q15 and Q16 *)

let rec list_lenght (l: 'a list): int =
	match l with
	| [] -> 0
	| e::r -> 1 + list_lenght r ;;

(* Q15. *)
let tourner_config ((ccl, cl, dim)): configuration =
	(* counts the number of players *)
	let rec list_lenght (l: 'a list): int =
		 match l with
		 | [] -> 0
		 | e::r -> 1 + list_lenght r
	(* computes the number of 6th of tours to change protagonist *)
	in let m = 6/(list_lenght cl)
	(* rotationg configuration by rotating each case *)
	in let rec tourner_ccl (l:case_coloree list): case_coloree list =
		match l with
		| [] -> []
		| (c,col)::r -> (tourner_case m c, col)::(tourner_ccl r)
	(* returns the configuration with every case rotated 
		 and the rotated list of players *)
	in (tourner_ccl ccl, tourner_list cl, dim) ;;

(* Q16. *)
let remplir_init (pl: couleur list) (dim: dimension): configuration =
	(* case_init is the top left case of the botton triangle 
		 in the board of dimension dim *)
	let case_init = ((-(dim+1)), 1, dim) in
	(* triangleBas_coloree returns the botton triangle 
		 from case_init colored with color c *)
	let triangleBas_coloree (c:couleur) = colorie c (remplir_triangle_bas dim case_init) in
	let rec get_conf_init (acc: couleur list) ((ccl,cl,dim): configuration): configuration =
		match acc with
		| [c]  -> tourner_config (triangleBas_coloree c @ ccl, cl, dim)
		| c::r -> get_conf_init r (tourner_config 
																(triangleBas_coloree c @ ccl,cl,dim)
															)
		| [] -> ([],pl,dim) (* This case avoids non exhaustive mathching warning *)
	in let (p1,rpl) = 
		match pl with 
			| [] -> failwith "Empty player list, possible number of players : 1,2,3,6"
			| c::r -> (c,r)
	in if rpl <> [] then
				get_conf_init rpl (tourner_config (triangleBas_coloree p1, pl, dim))
		 else (triangleBas_coloree p1,pl,dim) ;;
affiche (remplir_init [Code "Max"; Code "Raf"] 3);;

(*Q.17*)
let quelle_couleur (c:case)(conf:configuration):couleur = 
	let (ccol,_,_) = conf in 
		associe c ccol Libre ;;

(*Q.18*)
let supprime_dans_config (conf:configuration)(c:case):configuration = 
	if (quelle_couleur c conf) = Libre then conf 
	else let (ccol,pl,d) = conf in
		let rec search_case (casecol:case_coloree list)(c:case):case_coloree list = 
			match casecol with
			|[] -> failwith "can't be empty"
			|(case,col)::lp -> if c = case then lp else (case,col)::search_case lp c
		in (search_case ccol c, pl,d) ;;


(*Q.19*)
let est_coup_valide (conf:configuration)(co:coup):bool = 
	let (ccol,pl,d) = conf in 
	match co with
	|Du (c1,c2) -> (sont_cases_voisines c1 c2) && 
								 (quelle_couleur c1 conf) = der_list(tourner_list pl) && 
							   (quelle_couleur c2 conf ) = Libre && 
								 (est_dans_losange c2 d)
	|Sm lp 			-> failwith "Sauts multiples non implementés" ;;

(*Q.20*)
let appliquer_coup (conf:configuration)(cp:coup):configuration = match cp with
|Du (c1,c2) -> let coul = quelle_couleur c1 conf and 
							 conf2 = supprime_dans_config conf c1 in 
							 let (ccol,pl,d) = conf2 in 
							 ((c2,coul)::ccol,pl,d)
|Sm lp -> failwith "Sauts multiples non implementés" ;;

(*Q.21*)
let mettre_a_jour_conf (conf:configuration) (cp:coup):configuration = 
	if est_coup_valide conf cp then appliquer_coup conf cp 
	else failwith "Invalid move, please play again." ;; 

(* Q.22 *)
let est_libre_seg (c1:case)(c2:case)(conf:configuration):bool = 
	let (ci,cj,ck) = c1 in 
	let ((iv,jv,kv),d) = vec_et_dist c1 c2 in
	let rec est_libre_case (cs:case)(dis:int):bool = 
		let (i,j,k) = cs in 
			if dis = 0 then true 
			else (quelle_couleur cs conf) = Libre &&
						est_libre_case (i+iv,j+jv,k+kv)(dis-1)
	in est_libre_case (ci+iv,cj+jv,ck+kv)(d-1) ;;   

(*Q.23*)
let est_saut (c1:case) (c2:case) (conf:configuration):bool = 
	let pivot = calcul_pivot c1 c2 in
	match pivot with
	| None -> false
	| Some pivot -> (quelle_couleur pivot conf <> Libre) &&
									(quelle_couleur c2 conf = Libre) &&
									est_libre_seg c1 pivot conf &&
				    			est_libre_seg pivot c2 conf ;;

(*Q.24*)
let rec est_saut_multiple (c:case list)(conf:configuration):bool = 
	match c with
	|[]-> failwith "Can't operate on an empty list"
	|a::[] -> failwith "You need two cases at least to do a jump"
	|a::(b::[]) -> est_saut a b conf
	|a::(b::lp) -> est_saut a b conf && est_saut_multiple (b::lp) conf ;;

(*Q.25*)
let est_coup_valide (conf:configuration)(co:coup):bool = 
	let (ccol,pl,d) = conf in 
	match co with
		|Du (c1,c2) -> (sont_cases_voisines c1 c2) && 
									 (quelle_couleur c1 conf) = der_list(tourner_list pl) && 
									 (quelle_couleur c2 conf ) = Libre && 
									 (est_dans_losange c2 d)
		|Sm lp 			-> let final_el = der_list lp and 
									 conf_saut = 
									 		supprime_dans_config conf (der_list (tourner_list lp)) in 
									 (est_dans_losange final_el d) &&
									 (est_saut_multiple lp conf_saut) ;; 

let appliquer_coup (conf:configuration)(cp:coup):configuration = 
	match cp with
	|Du (c1,c2) -> let coul = quelle_couleur c1 conf and 
								 conf2 = supprime_dans_config conf c1 in 
								 let (ccol,pl,d) = conf2 in 
										((c2,coul)::ccol,pl,d)
	|Sm (a::lp) -> let coul = quelle_couleur a conf and 
								 final_el = der_list lp and 
								 (ccol2,pl2,d2) = supprime_dans_config conf a in 
										((final_el,coul)::ccol2,pl2,d2) 
	|Sm [] -> conf ;;	(* Obs: If not added, the absence of this last case raises 
											a not exhaustive pattern matching warning, even though
											this case should be treated by the fun. est_coup_valide *)

let mettre_a_jour_conf (conf:configuration) (cp:coup):configuration = 
	if est_coup_valide conf cp then appliquer_coup conf cp 
	else failwith "Invalid move, please play again." ;;
	
(* Q26. *)
let score ((ccl,pl,_): configuration): int =
	let protagonist = List.hd pl in
	List.fold_left (fun acc cc -> let ((i,_,_), col)=cc in
																if col = protagonist then i+acc
																else 0+acc
								 ) 0 ccl ;;

let score_gagnant (dim:dimension): int =
	(dim * dim * dim) + (dim * dim) + (dim * (dim + 1) * ((-dim)+1)) / 3 ;;

let score_gagnant_fold (dim:dimension): int =
	List.fold_left (
									fun acc c -> let (i,_,_) = c in i+acc
								 ) 0 (remplir_triangle_haut dim ((dim+1), (-dim), (-1))) ;;

(* Q27. *)
let gagne ((ccl,pl,dim): configuration): bool = 
	score (ccl,pl,dim) = score_gagnant dim ;;

(* Q28. *)
let est_partie (conf:configuration) (cpl:coup list): couleur =
	(* Compute all coups and update conf in order (left to right) *)
	let (fin_ccl,pl,dim) = List.fold_left (
			fun act_conf cp -> tourner_config (mettre_a_jour_conf act_conf cp)
		) conf cpl
	(* update conf *)
	in let conf = (fin_ccl,pl,dim) 
	(* Iterates through list of player veryfing if they have won *)
	in let winner = List.fold_left ( 
			(* takes the couple (acc_conf,acc_winner) containing the track of the
				rotated conf and the actual winner if there is. col is the player 
				fold_left is iterating at *)
			fun (acc_conf, acc_winner) col ->
				(* if the actual player wins then update accumulator *)
				if gagne (conf) then (tourner_config acc_conf, col)
				(* if the actual player does not win and theres a winner in the 
					 accumulator it keeps the value recorded *)
				else if acc_winner <> Libre then (tourner_config acc_conf,acc_winner) 
				(* if the actual player does not win and theres no winner yet update the
					 accumulator with the default value *)
				else (conf, Libre)
		) (conf, Libre) (pl) 
	in snd(winner);;

let conf = remplir_init [Code "p1"] 1 ;;
affiche conf;;
est_partie conf [
										Du(((-2),1,1),((-1),1,0)); 
										Du(((-1),1,0),(0,0,0)); 
										Du((0,0,0),(1,(-1),0)); 
										Du((1,(-1),0),(2,(-1),-1))
									] ;;
(* Output : - : couleur = Code "p1" *)

(* Q29. *)

let cases_voisines ((i,j,k):case): case list =
	(* neighboring cases of the given case *)
	[
		(i+1, j-1, k); (i+1, j, k-1); (i, j+1, k-1); 
		(i, j-1, k+1); (i-1, j+1, k); (i-1, j, k+1)
	 ] ;;

let coups_unitaires_possibles (conf: configuration) (c:case): coup list =
	List.fold_left (
		fun acc cv -> if est_coup_valide conf (Du(c,cv)) then (Du(c,cv))::acc 
									else acc
	) [] (cases_voisines c) ;;

coups_unitaires_possibles (remplir_init [Code "bob";Code "Ana"] 3) ((-4),1,3) ;;

let rec get_sautSimple_direction ((ccl,pl,dim):configuration) (c1:case) (c2:case) (vect:vecteur): case option =
	let cp = (translate c2 vect) in
	if est_dans_losange cp dim then
		if est_saut c1 cp (ccl,pl,dim) then Some cp
		else get_sautSimple_direction (ccl,pl,dim) c1 cp vect
	else None ;;

get_sautSimple_direction (remplir_init [Code "bob";Code "Ana"] 3) ((-5),3,2) ((-4),3,1) (fst(vec_et_dist ((-5),3,2) ((-4),3,1)));;

let sauts_simples_possibles ((ccl,pl,dim): configuration) (c: case): case list list =
	let cases_voisines = cases_voisines c
	in List.fold_left (
		fun acc cv -> let vect_dest = fst (vec_et_dist c cv) in 
			let dest = get_sautSimple_direction (ccl,pl,dim) c (translate c vect_dest) vect_dest in
			match dest with
			| None -> acc
			| Some dest -> ([c;dest])::acc 
	) [] cases_voisines ;;

sauts_simples_possibles (remplir_init [Code "bob";Code "Ana"] 3) ((-5),3,2) ;;


let rec sauts_possibles_case_list ((ccl,pl,dim):configuration) (c:case) : case list list =
	let sauts_simples_de_c = sauts_simples_possibles (ccl,pl,dim) c in
	List.fold_left (
		fun acc cl ->
			acc @
			List.map (fun scl -> (List.hd cl)::scl) (sauts_possibles_case_list (((der_list cl), Code "tmp")::ccl,pl,dim) (der_list cl))
	) sauts_simples_de_c sauts_simples_de_c;;

let caseListList_to_sautList (sl:case list list): coup list =
	List.map (fun cl -> Sm cl) sl ;;

let sauts_possibles (conf:configuration) (c:case): coup list = 
	caseListList_to_sautList (sauts_possibles_case_list conf c) ;;

let conf_test = ([((4, -1, -3), Code "Ana"); ((4, -2, -2), Code "Ana");
((4, (-3), (-1)), Code "Ana"); ((5, (-2), (-3)), Code "Ana");
((5, (-3),(-2)), Code "Ana"); ((6, (-3), (-3)), Code "Ana");
(((-2), 3, (-1)), Code "bob"); (((-4),2,2), Code "bob");
(((-4), 3, 1), Code "bob"); ((0, 2, (-2)), Code "bob");
(((-5), 3, 2), Code "bob"); (((-6), 3, 3), Code "bob")],
[Code "bob"; Code "Ana"], 3) ;;

sauts_possibles conf_test ((-5),3,2) ;;

let coup_possibles (conf: configuration) (c:case): (case*coup) list =
	let cp_list = (coups_unitaires_possibles conf c) @ (sauts_possibles conf c)
	in List.map (fun cp -> 
			match cp with 
			| Du (c1,c2) -> (c2,cp)
			| Sm cl -> ((der_list cl), (Sm cl))
		) cp_list  ;;

coup_possibles conf_test ((-5),3,2);;

(* Q30. *)

(* 
	 get all cases of player 
	 returnthe coup that returns the max of the coups that gives a max score from each case
	 *)

let max_score_coup_list (cpl: (case*coup) list): coup =
	let dest_list = List.map (fun ccp -> fst ccp) cpl in
	let max_score_dest = snd (List.fold_left (
		fun (max, cp) c_dest ->
			let (i,_,_)=c_dest in if i > max then (i,c_dest) else (max,cp)
	) (0,List.hd dest_list) dest_list)
	in snd (List.find (fun (c,cp) -> c = max_score_dest) cpl);;

let coup_list_to_case_coup_list (cpl: coup list): (case*coup) list = 
	List.map (
		fun cp -> 
			match cp with
			| Du (c1,c2) -> (c2, Du(c1,c2))
			| Sm cl -> ((der_list cl), Sm cl)
	) cpl ;;

let strategie_gloutonnne ((ccl,pl,dim):configuration): coup =
	let protagonist = List.hd pl in
	let protagonist_cases = List.filter (fun (c,col) -> col = protagonist) ccl in
	let max_score_coup_per_case = List.fold_left (
		fun max_list c ->
			max_score_coup_list (coup_possibles (ccl,pl,dim) c) :: max_list
	) [] (List.map (fun (c,col) -> c) protagonist_cases)
	in let max_coup_list = coup_list_to_case_coup_list max_score_coup_per_case
	in max_score_coup_list max_coup_list;; 

strategie_gloutonnne (conf_test) ;;
