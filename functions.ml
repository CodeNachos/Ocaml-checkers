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
