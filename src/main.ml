

(*--------------------------------Définition des types et exceptions--------------------------------*)


type attribut = Victoire | Tresor | Action;;
type phase = Action | Achat;;
type carte = {cost : int; attribut : attribut; effect : int}
(*On relie l'entier de effect a un tableau qui stock les effets de carte sous forme
de liste de fonctions "player -> player" à executer*);;
type paquet = {contents : string list ; nbr : int};;
type player_state = {mutable actions : int ;mutable tresors :int ;mutable achats : int;mutable cp : phase};;
type player = {mutable deck : paquet ;mutable hand : string list ;mutable discard : paquet ;mutable played : string list;mutable ps : player_state};;
type board = {market : (string, int) Hashtbl.t ; trash : paquet};;
exception BreakPlayer of player;;
exception BreakPlayerScore of (player*int);;
exception BreakPlayers of (player*player);;
exception BreakPlayersTurn of (player*player*int);;
					

(*Ensemble des cartes*);;
let cuivre = {cost=0 ; attribut = Tresor ; effect = 1};;
let argent = {cost=3 ; attribut = Tresor ; effect = 7};;
let domaine = {cost=2 ; attribut = Victoire ; effect = 0};;
let duche = {cost=5 ; attribut = Victoire ; effect = 0};;
let forgeron = {cost=4 ; attribut = Action ; effect = 2};;
let village = {cost=3 ; attribut = Action ; effect = 3};;
let festival = {cost=5 ; attribut = Action ; effect = 4};;
let laboratoire = {cost=5 ; attribut = Action ; effect = 5};;
let marche = {cost=5 ; attribut = Action ; effect = 6};;
let province = {cost=8 ; attribut = Victoire ; effect = 0};;
let au = {cost=6 ; attribut = Tresor ; effect = 8};;




let boite = Hashtbl.create 42;;
Hashtbl.add boite "Cuivre" cuivre;;
Hashtbl.add boite "Domaine" domaine;;
Hashtbl.add boite "Forgeron" forgeron;;
Hashtbl.add boite "Village" village;;
Hashtbl.add boite "Festival" festival;;
Hashtbl.add boite "Laboratoire" laboratoire;;
Hashtbl.add boite "Marche" marche;;
Hashtbl.add boite "Duche" duche;;
Hashtbl.add boite "Argent" argent;;
Hashtbl.add boite "Province" province;;
Hashtbl.add boite "Or" au;;




(*--------------------------------Fonctions de base--------------------------------*)

let copie_player player =
	let new_ps = {actions = player.ps.actions; tresors = player.ps.tresors; achats=player.ps.achats;cp=player.ps.cp} in
	let result = {deck = player.deck;hand = player.hand;discard = player.discard;played=player.played;ps=new_ps}
	in result;;

let get_attribut name = (Hashtbl.find boite name).attribut;;
let get_effect name = (Hashtbl.find boite name).effect;;
let get_cost name = (Hashtbl.find boite name).cost;;

let create_setup lst =
	let setup = Hashtbl.create 20 in
	Hashtbl.add setup "Cuivre" 46;
	Hashtbl.add setup "Argent" 40;
	Hashtbl.add setup "Domaine" 8;
	Hashtbl.add setup "Duche" 8;
	Hashtbl.add setup "Province" 8;
	Hashtbl.add setup "Or" 30;
	List.iter(fun elem -> Hashtbl.add setup elem 10)lst;
	setup;;

let disp_market market = Hashtbl.iter(fun cle valeur -> Printf.printf "%s restants : %d\n" cle valeur) market;;
let incr_tres player = player.ps.tresors <- player.ps.tresors +1;player;;
let incr_act player = player.ps.actions <- player.ps.actions +1;player;;
let incr_ach player = player.ps.achats <- player.ps.achats +1;player;;
let decr_act player = player.ps.actions <- player.ps.actions -1;player;;
let close_action_phase player = player.ps.cp <- Achat;player;;
 
  
let shuffle_discard player = 
(*Quand on appelle cette fonction le deck est vide*)
	let new_deck = {contents = shuffle_lst player.discard.contents ; nbr = player.discard.nbr} in
	player.deck <- new_deck;
	player.discard <- {contents = []; nbr = 0};
	player;;

let rec draw player = match player.deck.contents with
(*Pioche une carte : prend la carte du debut de liste et l'ajoute a la main*)
	|h::t -> let new_deck = {contents = t; nbr = player.deck.nbr - 1} in
				player.deck <- new_deck;
				player.hand <- h::player.hand;
				player;
	|[] -> match player.discard.contents with
		|[] -> player
		|_ -> draw (shuffle_discard player);;
		
let take market player card_name =
(*Modifie in place*)
	let restants = Hashtbl.find market card_name in
	if restants <> 0 then
	begin
		Hashtbl.replace market card_name (restants-1);
		let new_discard = {contents = card_name::player.discard.contents ; nbr = player.discard.nbr+1} in
		player.discard <- new_discard;
		player;
	end
	else (disp_market market; failwith "take : Forbbiden take");;

let buy market player card_name affiche =
(*Modifie in place*) 
	let curr_cost = (Hashtbl.find boite card_name).cost in
	if curr_cost <= player.ps.tresors && player.ps.achats <> 0 then
	(*take s'occupera de verifier si il y a assez de cartes au marche*)
	begin
		if affiche then Printf.printf "	Achat : %s \n" card_name;
		take market player card_name;
		player.ps.achats <- player.ps.achats-1;
		player.ps.tresors <- player.ps.tresors-curr_cost;
		player; 
	end
	else failwith "buy : Forbbiden buy";;
				
let hand_to_played player card_name =
(*La carte doit etre dans la main du joueur*)
	player.hand <- (let rec aux = function
							|h::t when h=card_name -> t
							|h::t -> h::(aux t)
							|[] -> []
						in aux player.hand);
	player.played <- card_name::player.played;
	player;;



(*Effets des cartes*)
let effects = [|
[];
[incr_tres];
[draw;draw;draw];
[draw;incr_act;incr_act];
[incr_act;incr_act;incr_ach;incr_tres;incr_tres];
[draw;draw;incr_act];
[draw;incr_act;incr_ach;incr_tres];
[incr_tres;incr_tres];
[incr_tres;incr_tres;incr_tres];
|];;



let play player card_name affiche=
(*Créé un nouveau player*)
	let attr = get_attribut card_name in
	if attr = Victoire then failwith "play : Victory cards aren't playable" else
	if List.exists(fun name -> name = card_name) player.hand then
		if attr = Action then
			if player.ps.cp = Action && player.ps.actions > 0 then
			begin
				let tpm = ref player in
				List.iter (fun fct -> tpm := fct !tpm) effects.(get_effect card_name);
				if affiche then Printf.printf "	Action : %s \n" card_name;
				hand_to_played (decr_act !tpm) card_name;
			end
			else failwith "play : Not the right phase or not action points remaining"
		else 
			if player.ps.cp = Achat then
				begin
				let tpm = ref player in
				List.iter (fun fct -> tpm := fct !tpm) effects.(get_effect card_name);
				if affiche then Printf.printf "	Tresor : %s \n" card_name;)
				hand_to_played !tpm card_name;
				end
			else failwith "play : Not the right phase"
	else failwith "play : Not in hand";;


let play_all_treasures player affiche=
(*Créé un nouveau player*)
	let tpm = ref player in
	List.iter (fun str -> if get_attribut str = Tresor then tpm := play !tpm str affiche) player.hand;
	!tpm;;

let discard player lst=
	List.iter(fun str-> player.discard <- {contents = str::player.discard.contents;
														nbr = player.discard.nbr +1})lst;
	player;;


let end_of_turn player = 
	let tpm = discard(discard player player.played)player.hand in
	tpm.hand <- [];tpm.played <- [];tpm.ps <- {actions = 1;
															 tresors = 0;
															 achats = 1;
															 cp = Action};
	draw(draw(draw(draw(draw tpm))));;




(*--------------------------------outils pour l'IA--------------------------------*)

let eval player=
(*L'evaluation doit se faire apres un tour*)
	let count = ref 0 in
	let rec aux lst =
		List.iter(fun str -> if get_attribut str = Victoire then 
							match str with
							|"Domaine" -> incr(count)
							|"Duche" -> count := !count +3
							|"Province" -> count := !count +6
							|_ -> failwith "Erreur dans eval : Une carte Victoire ne passe pas le filtrage") lst in
	aux player.hand;
	aux player.deck.contents;
	aux player.discard.contents;
	(!count);;


let how_much_actions player =
	let count = ref 1 in (*Action "passer en phase d'achat"*)
	List.iter (fun name -> if get_attribut name = Action then incr(count)) player.hand;
	!count;;

let arractions n player =
	let result = Array.make n "Nothing" in
	let i = ref 0 in
	List.iter (fun name -> if get_attribut name = Action then (incr(i);result.(!i) <- name)) player.hand;
	result;;
	

(*Fonctionnement différent de ses prédécesseurs*)
(*A executer en debut de partie*)

(*Seulement utiles dans tableau de choix dachat*)
let how_much_buy board bourse=
	let count = ref 1 in (*Action "ne rien faire"*)
	Hashtbl.iter (fun name _ -> if name <> "Cuivre" && get_cost name <= bourse then incr(count)) board.market;
	!count;;
	
let arrachats n board bourse= 
	let result = Array.make n "Nothing" in
	let i = ref 0 in
	Hashtbl.iter (fun name _ -> if name <> "Cuivre" && get_cost name <= bourse then (incr(i);result.(!i) <- name)) board.market;
	result;;

let tableau_de_choix_dachat board=
	(*result[i] indique ce que je peux acheter avec i pieces (cuivres exclus)*)
	let result = Array.make 10 [||] in 
	(*La derniere case contient un tableau d'une case : le nombre de paquets vide,
	à transformer en int pour utilisation (à cause des problemes de type)*)
	for i=0 to 8 do
		result.(i) <- arrachats (how_much_buy board i) board i
	done;
	result.(9) <- [|"0"|];
	result;;
		
let simuler ai afficher=
	(*On simule jusqu'à que la pile des Provinces soit vide ou que 3 piles soient vides*)
	(*Creation de la partie*)
	try
	let depart = { contents = shuffle_lst (List.map(fun i -> if i <= 6 then "Cuivre" else "Domaine")
											(List.init 10 (fun i->i)));
				nbr = 10} in
	let beginning_ps = {actions = 1 ; tresors = 0 ; achats = 1; cp = Action} in
	let player = {deck = depart; hand = []; discard = {contents = []; nbr = 0};played = []; ps = beginning_ps} in
	let setup = create_setup ["Forgeron";"Village";"Marche";"Laboratoire";"Festival"] in
	let board = {market=setup;trash ={contents=[];nbr=0}} in
	let choix_d_achats = tableau_de_choix_dachat board in
	let player_ref = ref (draw(draw(draw(draw(draw(player)))))) in
	(*On commence !*)
	for i=1 to 10000 do
		if afficher then Printf.printf "Tour %d \n" i;
		player_ref := (end_of_turn (ai !player_ref board choix_d_achats afficher));
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayerScore (!player_ref,i));
	done;
	!player_ref,10000;
	with
		|BreakPlayerScore a -> a;;

let simule2 ai1 ai2 afficher =
	(*On simule jusqu'e que la pile des Province soit vide ou que 3 piles soient vides*)
	(*Creation de la partie*)
	try
	(*Initialisation des deux joueurs, obligé de séparer car mutable*)
	let depart1 = { contents = shuffle_lst (List.map(fun i -> if i <= 6 then "Cuivre" else "Domaine")
											(List.init 10 (fun i->i)));
				nbr = 11} in
	let beginning_ps1 = {actions = 1 ; tresors = 0 ; achats = 1; cp = Action} in
	let player1 = {deck = depart1; hand = []; discard = {contents = []; nbr = 0};played = []; ps = beginning_ps1} in
	let depart2 = { contents = shuffle_lst (List.map(fun i -> if i <= 6 then "Cuivre" else "Domaine")
											(List.init 10 (fun i->i)));
				nbr = 10} in
	let beginning_ps2 = {actions = 1 ; tresors = 0 ; achats = 1; cp = Action} in
	let player2 = {deck = depart2; hand = []; discard = {contents = []; nbr = 0};played = []; ps = beginning_ps2} in
	
	let setup = create_setup ["Forgeron";"Village";"Marche";"Laboratoire";"Festival"] in
	let board = {market=setup;trash ={contents=[];nbr=0}} in
	let choix_d_achats = tableau_de_choix_dachat board in
	let player_ref1 = ref (draw(draw(draw(draw(draw(player1)))))) in
	let player_ref2 = ref (draw(draw(draw(draw(draw(player2)))))) in
	(*On commence !*)
	for i=1 to 10000 do
		if afficher then Printf.printf "Tour %d \nJoueur 1 :\n" i;
		player_ref1 := (end_of_turn (ai1 !player_ref1 board choix_d_achats afficher !player_ref2));
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayers (!player_ref1,!player_ref2));
		if afficher then Printf.printf "Joueur 2 :\n";
		player_ref2 := (end_of_turn (ai2 !player_ref2 board choix_d_achats afficher !player_ref1));
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayers (!player_ref1,!player_ref2));
	done;
	(!player_ref1,!player_ref2);
	with
		|BreakPlayers a -> a;;


(*--------------------------------Glouton + fonctions fondatrices des IA--------------------------------*)

let glouton player board _ afficher _ =
(*Prends un player en debut de tour*)
(*Ne gère pas s'il manque des cartes dans la pile*)
	let tpm = play_all_treasures (close_action_phase player) afficher in
	if tpm.ps.tresors >= 8 then buy board.market tpm "Province" afficher
	else if tpm.ps.tresors >= 6 then buy board.market tpm "Or" afficher
	else if tpm.ps.tresors >= 3 then buy board.market tpm "Argent" afficher
	else tpm;;

				
let aleatoire player board choix_d_achats affiche _ =
	(*Ici on modifie le choix_d_achats*)
	let player_ref = ref player in
	while !player_ref.ps.cp = Action && !player_ref.ps.actions <> 0 do
		let n = how_much_actions !player_ref in
		let arr = arractions n !player_ref in
		let choice = Random.int n in
		if choice = 0 then !player_ref.ps.cp <- Achat
		else
		begin
			player_ref := play !player_ref arr.(choice) affiche;
		end
	done;
	!player_ref.ps.cp <- Achat;
	player_ref := play_all_treasures !player_ref affiche;
	while !player_ref.ps.cp = Achat && !player_ref.ps.achats <> 0 do
		let bourse = if !player_ref.ps.tresors >= 8 then 8 else !player_ref.ps.tresors in
		let arr = choix_d_achats.(bourse) in
		let n = Array.length arr in
		let choice = Random.int n in
		if choice = 0 then !player_ref.ps.cp <- Action
		else 
			begin
			buy board.market !player_ref arr.(choice) affiche;
			if Hashtbl.find board.market arr.(choice) = 0 then
				begin
				for i=0 to 8 do
					choix_d_achats.(i) <- array_filter (fun str -> str <> arr.(choice)) choix_d_achats.(i);
				done;
				choix_d_achats.(9).(0) <- string_of_int (int_of_string choix_d_achats.(9).(0) + 1);
				end
			end
	done;
	!player_ref;;

let update memoire score=
	let liste = !memoire in
	if List.hd liste > score then
		begin
		let rec placement score = function
			|h::t when h > score -> h::(placement score t)
			|h::t -> score::h::t
			|[] -> [score]
		in
		let presult = placement score liste in
		memoire := List.tl presult;
		end;;
		
let rec sum_lst = function
	|h::t -> h + sum_lst t
	|[] -> 0;;
	
let simule_mt player board choix_d_achats adv =
	try
	let player_ref = ref player in
	let adv_ref = ref adv in 
	let i = ref 0 in
	while true do 
		player_ref := (end_of_turn (aleatoire !player_ref board choix_d_achats false adv));(*adv dans aleatoire est la juste pour le typage, inutile sinon*)
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayersTurn (!player_ref,!adv_ref,!i));
		adv_ref := (end_of_turn (aleatoire !adv_ref board choix_d_achats false adv));(*adv dans aleatoire est la juste pour le typage, inutile sinon*)
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayersTurn (!player_ref,!adv_ref,!i));
		incr(i)
	done;
	!player_ref,!adv_ref,!i;
	with
		|BreakPlayersTurn a -> a;;


let simule_n_mt player board choix_d_achats n adv=
	let taille = (n/10)+1 in (*On prends les 10 pourcents les plus petits*)
	let memoire = ref (List.init taille (fun _ -> 300) ) in
	for i=0 to (n-1) do
		let new_board = {market = Hashtbl.copy board.market;trash = {contents=[];nbr = 0}} in
		player.deck <- {contents=shuffle_lst player.deck.contents; nbr = player.deck.nbr};
		adv.deck <- {contents=shuffle_lst adv.deck.contents; nbr = adv.deck.nbr};
		let p1,p2,t = simule_mt (copie_player player) new_board (Array.map Array.copy choix_d_achats)(copie_player adv) in
		let score = t + (if (eval p2 - eval p1) < 0 then -100 else 100) in
		update memoire score;
	done;
	sum_lst !memoire;;

(*NE S'ADAPTE PAS A TOUS LES BOARDS*)
let quickplay player affiche =
	let to_play = ref "Nothing" in
	List.iter (fun str ->
		if get_attribut str = Action then
			begin
			if !to_play = "Nothing" then
				to_play := str
			else if !to_play = "Forgeron" then
				to_play := str
			end
		 ) player.hand;
	if !to_play = "Nothing" then (player.ps.cp <- Achat;player)
	else play player !to_play affiche;
	;;

let aleatoire_actionless player board choix_d_achats affiche _=
	(*Ici on modifie le choix_d_achats*)
	let player_ref = ref player in
	while !player_ref.ps.cp = Action && !player_ref.ps.actions <> 0 do
		player_ref := quickplay !player_ref affiche;
	done;	
	!player_ref.ps.cp <- Achat;
	player_ref := play_all_treasures !player_ref affiche;
	while !player_ref.ps.cp = Achat && !player_ref.ps.achats <> 0 do
		let bourse = if !player_ref.ps.tresors >= 8 then 8 else !player_ref.ps.tresors in
		let arr = choix_d_achats.(bourse) in
		let n = Array.length arr in
		let choice = Random.int n in
		if choice = 0 then !player_ref.ps.cp <- Action
		else 
			begin
			buy board.market !player_ref arr.(choice) affiche;
			if Hashtbl.find board.market arr.(choice) = 0 then
				begin
				for i=0 to 8 do
					choix_d_achats.(i) <- array_filter (fun str -> str <> arr.(choice)) choix_d_achats.(i);
				done;
				choix_d_achats.(9).(0) <- string_of_int (int_of_string choix_d_achats.(9).(0) + 1);
				end
			end
	done;
	!player_ref;;

let simule_mt_actionless player board choix_d_achats adv=
	try
	let player_ref = ref player in
	let adv_ref = ref adv in
	let i = ref 0 in
	while true do 
		player_ref := (end_of_turn (aleatoire_actionless !player_ref board choix_d_achats false adv ));(*adv dans aleatoire est la juste pour le typage, inutile sinon*)
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayersTurn (!player_ref,!adv_ref,!i));
		adv_ref := (end_of_turn (aleatoire !adv_ref board choix_d_achats false adv));(*adv dans aleatoire est la juste pour le typage, inutile sinon*)
		if Hashtbl.find board.market "Province" = 0 ||
		int_of_string choix_d_achats.(9).(0) >= 3 then raise (BreakPlayersTurn (!player_ref,!adv_ref,!i));
		incr(i);
	done;
	!player_ref,!adv_ref,!i;
	with
		|BreakPlayersTurn a -> a;;

let simule_n_mt_actionless player board choix_d_achats n adv =
	let taille = (n/10)+1 in (*On prends les 10 pourcents les plus petits*)
	let memoire = ref (List.init taille (fun _ -> 300) ) in
	for i=0 to (n-1) do
		let new_board = {market = Hashtbl.copy board.market;trash = {contents=[];nbr = 0}} in
		player.deck <- {contents=shuffle_lst player.deck.contents; nbr = player.deck.nbr};
		adv.deck <- {contents=shuffle_lst adv.deck.contents; nbr = adv.deck.nbr};
		let p1,p2,t = simule_mt_actionless (copie_player player) new_board (Array.map Array.copy choix_d_achats)(copie_player adv) in
		let score = t + if eval p2 - eval p1 < 0 then -100 else 100 in
		update memoire score;
	done;
	sum_lst !memoire;;
	
(*--------------------------------Monte-Carlo--------------------------------*)


let monte_carlo player board choix_d_achats affiche adv=
	(*5 occurences à changer pour le nombre de simulations*)
	let player_ref = ref player in
	let adv_av = ref (copie_player adv) in 
	while !player_ref.ps.cp = Action && !player_ref.ps.actions <> 0 do
		let n = how_much_actions !player_ref in
		let arr = arractions n !player_ref in
		let best = ref 300000000 in
		let ibest = ref (-1) in
		for i=0 to (n-1) do
			let player_av= ref (copie_player !player_ref) in
			if i = 0 then 
				begin
				!player_av.ps.cp <- Achat;
				let score = simule_n_mt !player_av board choix_d_achats 500 !adv_av in
				if score <= !best then (best := score;ibest := i;)
				end
			else
				begin
				player_av := play !player_av arr.(i) false;
				let score = simule_n_mt !player_av board choix_d_achats 500 !adv_av in
				if score <= !best then (best := score;ibest := i;)
				end
		done;
		let choice = !ibest in
		if choice = 0 then !player_ref.ps.cp <- Achat
		else
		begin
			player_ref := play !player_ref arr.(choice) affiche;
		end
	done;	
	!player_ref.ps.cp <- Achat;
	player_ref := play_all_treasures !player_ref affiche;
	while !player_ref.ps.cp = Achat && !player_ref.ps.achats <> 0 do
		let bourse = if !player_ref.ps.tresors >= 8 then 8 else !player_ref.ps.tresors in
		let arr = choix_d_achats.(bourse) in
		let n = Array.length arr in
		let best = ref 300000000 in
		let ibest = ref (-1) in
		for i=0 to (n-1) do
			let player_av= ref (copie_player !player_ref) in
			if i = 0 then 
				begin
				!player_av.ps.cp <- Action;
				let score = simule_n_mt !player_av board choix_d_achats 500 !adv_av in
				if score <= !best then (best := score;ibest := i;)
				end
			else 
				begin
				buy board.market !player_av arr.(i) false;
				if Hashtbl.find board.market arr.(i) = 0 then
					begin
					let new_choixd = Array.make 10 [||] in
					for k=0 to 8 do (*non modifie*)
						new_choixd.(k) <- array_filter (fun str -> str <> arr.(i)) choix_d_achats.(k);
					done;
					new_choixd.(9) <- [|string_of_int (int_of_string choix_d_achats.(9).(0) + 1)|];
					let score = simule_n_mt !player_av board new_choixd 500 !adv_av in
					if score <= !best then (best := score;ibest := i;)
					end
				else (
					let score = simule_n_mt !player_av board choix_d_achats 500 !adv_av in
					if score <= !best then (best := score;ibest := i;)
				);
				Hashtbl.replace board.market arr.(i) (Hashtbl.find board.market arr.(i) + 1);
				end
		done;
		let choice = !ibest in
		if choice = 0 then !player_ref.ps.cp <- Action
		else 
			begin
			buy board.market !player_ref arr.(choice) affiche;
			if Hashtbl.find board.market arr.(choice) = 0 then
				begin
				for i=0 to 8 do
					choix_d_achats.(i) <- array_filter (fun str -> str <> arr.(choice)) choix_d_achats.(i);
				done;
				choix_d_achats.(9).(0) <- string_of_int (int_of_string choix_d_achats.(9).(0) + 1);
				end
			end
	done;
	!player_ref;;

let monte_carlo_actionless player board choix_d_achats affiche adv=
	(*POUR LE MOMENT NE MARCHE QUE SUR UN KINGDOM*)
	(*3 occurences à changer pour le nombre de simulations*)
	let player_ref = ref player in
	let adv_av = ref (copie_player adv) in
	while !player_ref.ps.cp = Action && !player_ref.ps.actions <> 0 do
		player_ref := quickplay !player_ref affiche;
	done;	
	!player_ref.ps.cp <- Achat;
	player_ref := play_all_treasures !player_ref affiche;
	while !player_ref.ps.cp = Achat && !player_ref.ps.achats <> 0 do
		let bourse = if !player_ref.ps.tresors >= 8 then 8 else !player_ref.ps.tresors in
		let arr = choix_d_achats.(bourse) in
		let n = Array.length arr in
		let best = ref 300000000 in
		let ibest = ref (-1) in
		for i=0 to (n-1) do
			let player_av= ref (copie_player !player_ref) in
			if i = 0 then 
				begin
				!player_av.ps.cp <- Action;
				let score = simule_n_mt_actionless !player_av board choix_d_achats 500 !adv_av in
				if score <= !best then (best := score;ibest := i;)
				end
			else 
				begin
				buy board.market !player_av arr.(i) false;
				if Hashtbl.find board.market arr.(i) = 0 then
					begin
					let new_choixd = Array.make 10 [||] in
					for k=0 to 8 do
						new_choixd.(k) <- array_filter (fun str -> str <> arr.(i)) choix_d_achats.(k);
					done;
					new_choixd.(9) <- [|string_of_int (int_of_string choix_d_achats.(9).(0) + 1)|];
					let score = simule_n_mt_actionless !player_av board new_choixd 500 !adv_av in
					if score <= !best then (best := score;ibest := i;)
					end
				else (
					let score = simule_n_mt_actionless !player_av board choix_d_achats 500 !adv_av in
					if score <= !best then (best := score;ibest := i;)
				);
				Hashtbl.replace board.market arr.(i) (Hashtbl.find board.market arr.(i) + 1);
				end
		done;
		let choice = !ibest in
		if choice = 0 then !player_ref.ps.cp <- Action
		else 
			begin
			buy board.market !player_ref arr.(choice) affiche;
			if Hashtbl.find board.market arr.(choice) = 0 then
				begin
				for i=0 to 8 do
					choix_d_achats.(i) <- array_filter (fun str -> str <> arr.(choice)) choix_d_achats.(i);
				done;
				choix_d_achats.(9).(0) <- string_of_int (int_of_string choix_d_achats.(9).(0) + 1);
				end
			end
	done;
	!player_ref;;

(*--------------------------------Monte-Carlo - UCB--------------------------------*)

let c = 15.;;

let rec sum_and_divide tab i j n =
	let a = float_of_int tab.(i) in
	let b = float_of_int n in
	if i = j then a/.b
	else a/.b +. sum_and_divide tab (i+1) j n;;

let avg_ten_percents tab tot =
	let taille = Array.length tab in
	if tot >= 10*taille then
		sum_and_divide tab 0 (taille-1) taille
	else
		if tot >= taille then
			sum_and_divide tab (taille-(max (tot/10) 1)) (taille-1) (max (tot/10) 1)
		else
			sum_and_divide tab (taille-(max (tot/10) 1)) (taille-1) (max (tot/10) 1);;
	
	
let swap arr i j = let tmp = arr.(j) in arr.(j) <- arr.(i); arr.(i) <- tmp;;
	
let updatab tab_tab_scores score=
	if tab_tab_scores.(0) > score then
		begin
			tab_tab_scores.(0) <- score;
			let i = ref 0 in
			let n = Array.length tab_tab_scores in
			while !i < n-1 && tab_tab_scores.(!i) < tab_tab_scores.(!i+1) do
				swap tab_tab_scores !i (!i+1);
				incr(i);
			done
		end;;	

let ucb10 choix tab_tab_scores nbrvisit gN = 
	avg_ten_percents tab_tab_scores.(choix) nbrvisit.(choix) 
	-. c*. sqrt(log(float_of_int(gN))/.float_of_int(nbrvisit.(choix)));;

	
let simule_mc_ucb_actionless player board choix_d_achats adv =
	let new_board = {market = Hashtbl.copy board.market;trash = {contents=[];nbr = 0}} in
	let nadv = copie_player adv in
	player.deck <- {contents=shuffle_lst player.deck.contents; nbr = player.deck.nbr};
	nadv.deck <- {contents=shuffle_lst adv.deck.contents; nbr = adv.deck.nbr};
	let p1,p2,t = simule_mt_actionless player new_board (Array.map Array.copy choix_d_achats) nadv in 
	let bonus = if eval p1 - eval p2 > 0 then -3 else (if eval p1 - eval p2 = 0 then 0 else 3 ) in
	let score = t -(eval p1)/6 + bonus in
	score;;


	
let mc_ucb_actionless player board choix_d_achats affiche adv=
	(*nombre représente le nombre de simus à faire par choix*)
	let nbr = 8000 in
	let player_ref = ref player in
	
	while !player_ref.ps.cp = Action && !player_ref.ps.actions <> 0 do
		player_ref := quickplay !player_ref affiche;
	done;	
	!player_ref.ps.cp <- Achat;
	player_ref := play_all_treasures !player_ref affiche;
	
	while !player_ref.ps.cp = Achat && !player_ref.ps.achats <> 0 do
		let bourse = if !player_ref.ps.tresors >= 8 then 8 else !player_ref.ps.tresors in
		let arr = choix_d_achats.(bourse) in
		let n = Array.length arr in
		let nbrvisit = Array.make n 1 in (*Grâce à la première boucle*)
		let ucb_tab = Array.make n 0. in
		let choice = ref 0 in
		let tab_tab_scores = Array.init n (fun _ -> Array.make (nbr/20) 300) in
		(*La simu*)
		if n <> 1 then 
			begin
			
			(*On fait une première boucle pour explorer au moins une fois*)
			for k = 0 to n-1 do
  				let player_av = ref (copie_player !player_ref) in
  				if k = 0 then
    				(!player_av.ps.cp <- Action;
    				let score = simule_mc_ucb_actionless !player_av board choix_d_achats adv in
    				updatab tab_tab_scores.(k) score)
  				else
    				begin
					buy board.market !player_av arr.(k) false;
					if Hashtbl.find board.market arr.(k) = 0 then
						begin
						let new_choixd = Array.make 10 [||] in
						for j=0 to 8 do
							new_choixd.(j) <- array_filter (fun str -> str <> arr.(k)) choix_d_achats.(j);
						done;
						new_choixd.(9) <- [|string_of_int (int_of_string choix_d_achats.(9).(0) + 1)|];
						let score = simule_mc_ucb_actionless !player_av board new_choixd adv in
						updatab tab_tab_scores.(k) score;
						end
					else
						begin
						let score = simule_mc_ucb_actionless !player_av board choix_d_achats adv in
						updatab tab_tab_scores.(k) score;
  						end;
					Hashtbl.replace board.market arr.(k) (Hashtbl.find board.market arr.(k) + 1);
					end;	
			done;
			for k = 0 to n-1 do
  				ucb_tab.(k) <- ucb10 k tab_tab_scores nbrvisit n
			done;
			
			for i=1 to nbr do
				let besti = array_min ucb_tab in
				let player_av = ref (copie_player !player_ref) in
				if besti = 0 then 
					(!player_av.ps.cp <- Action;
					let score = simule_mc_ucb_actionless !player_av board choix_d_achats adv in
					updatab tab_tab_scores.(besti) score;
					nbrvisit.(0) <- nbrvisit.(0) + 1;)
				else
					begin
					buy board.market !player_av arr.(besti) false;
					if Hashtbl.find board.market arr.(besti) = 0 then
						begin
						let new_choixd = Array.make 10 [||] in
						for k=0 to 8 do 
							new_choixd.(k) <- array_filter (fun str -> str <> arr.(besti)) choix_d_achats.(k);
						done;
						new_choixd.(9) <- [|string_of_int (int_of_string choix_d_achats.(9).(0) + 1)|];
						let score = simule_mc_ucb_actionless !player_av board new_choixd adv in
						nbrvisit.(besti) <- nbrvisit.(besti) + 1;
						updatab tab_tab_scores.(besti) score;
						end
					else
						begin
						let score = simule_mc_ucb_actionless !player_av board choix_d_achats adv in
						nbrvisit.(besti) <- nbrvisit.(besti) + 1;
						updatab tab_tab_scores.(besti) score;
						end;
					Hashtbl.replace board.market arr.(besti) (Hashtbl.find board.market arr.(besti) + 1);
					end;	
				for k=0 to (n-1) do
					ucb_tab.(k) <- ucb10 k tab_tab_scores nbrvisit (i+n) ; (*Ne pas oublier qu'on a exploré n fois au début !*)
				done;
			done;
			let tmp = Array.init n (fun i-> avg_ten_percents tab_tab_scores.(i) nbrvisit.(i)) in
			
			
			print_float_array tmp;
			print_float_array ucb_tab;
			print_int_array nbrvisit;
			print_string_array arr;
			
			
			
			choice := array_min tmp;
		end;
		if !choice = 0 then !player_ref.ps.cp <- Action
		else 
			begin
			buy board.market !player_ref arr.(!choice) affiche;
			if Hashtbl.find board.market arr.(!choice) = 0 then
				begin
				for i=0 to 8 do
					choix_d_achats.(i) <- array_filter (fun str -> str <> arr.(!choice)) choix_d_achats.(i);
				done;
				choix_d_achats.(9).(0) <- string_of_int (int_of_string choix_d_achats.(9).(0) + 1);
				end
			end;
	done;
	!player_ref;;


(*--------------------------------Quelques test--------------------------------*)

let j1,j2 = simule2 mc_ucb_actionless glouton true;;
let s1, s2 = eval j1,eval j2;;

let j1,j2 = simule2 monte_carlo_actionless glouton true;;
let s1, s2 = eval j1,eval j2;;
