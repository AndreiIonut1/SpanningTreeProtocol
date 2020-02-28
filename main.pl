%Determina nodul cu prioritatea minima din lista de noduri
%minPriority(NodesList, NodeToFind).
minPriority([[N,P]|T], Node) :- minPriorityAux(T, [N,P], Node).

%Determina nodul cu prioritatea minima din lista de noduri.
%Al doilea argument este un acumulator, pentru ca functia sa fie tail recursive
%minPriorityAux(List, Acc, Result).
minPriorityAux([], [N,_], N) :- !.
minPriorityAux([[N, P]|T], [_, MinP], Answer) :- P =< MinP, minPriorityAux(T, [N,P], Answer), !.
minPriorityAux([[_, _]|T], [MinN, MinP], Answer) :-  minPriorityAux(T, [MinN, MinP], Answer).

%Gaseste elementul de pe o anumita pozitie din lista si il pune in Result. Primul element are indexul 1.
%getElem(List, Pos, Result).
getElem([H|_], 1, H) :- !.
getElem([_|T], C, E) :- NewC is C - 1, getElem(T, NewC, E).

%Seteaza elementul de pe o anumita pozitie din lista la o anumita valoare. Primul element are indexul 1.
%setElem(List, Pos, Elem, ListResult).
setElem([_|T], 1, X, [X|T]) :- !.
setElem([H|T], Pos, X, [H|R]):- Pos > 1, NewPos is Pos - 1, setElem(T, NewPos, X, R).

%Initializeaza vectorul de distante la infinit si vectorul de parinti la 0(primul nod se va considera mereu 1).
%setStartVectors(Nodes, DistanceVector, PreviousVector).
setStartVectors([], [], []) :- !.
setStartVectors([[_,_] | T], Dist, Previous) :- 
		setStartVectors(T, DistAux, PreviousAux), 
		Dist = [99999 | DistAux], 
		Previous = [0 | PreviousAux].

%Intoarce nodul(si distanta lui) din lista de noduri care are distanta minima in vectorul de distante 
%getNodeWithMinDistance(Nodes, Distances, Node, Dist)
getNodeWithMinDistance([[N, _] | T], Distances, MinNode, MinDist) :- 
	getElem(Distances, N, D),
	getNodeWithMinDistanceAux(T, Distances, D, N, MinDist, MinNode).

%Functie auxiliara, foloseste 2 acumulatori pentru eficienta(este tail recursive).
%Un acumulator este pentru nodul cu distanta minima, al doilea este pentru distanta minima.
%getNodeWithMinDistanceAux(Nodes, Distances, DistAcc, NodeAcc, DistAnswer, NodeAnswer).
getNodeWithMinDistanceAux([], _, MinDist, MinNode, MinDist, MinNode) :- !. %pune acumulatorul in rezultat
getNodeWithMinDistanceAux([[N, _] | T], Distances, MinDist, _, DistAnswer, NodeAnswer) :- 
	getElem(Distances, N, Dist), %obtine distanta pentru nodul curent
	Dist =< MinDist, % compara cu minimul
	getNodeWithMinDistanceAux(T, Distances, Dist, N, DistAnswer, NodeAnswer). %actualizeaza noul minim
getNodeWithMinDistanceAux([[_, _] | T], Distances, MinDist, Node, DistAnswer, NodeAnswer) :- 
	getNodeWithMinDistanceAux(T, Distances, MinDist, Node, DistAnswer, NodeAnswer).

%Sterge un nod din lista de noduri 
%remove(Nodes, ElemToRemove, Result).
remove(Nodes, Elem, Result) :- removeAux(Nodes, Elem, [], Result).

%Functie auxiliara tail recursive.
%removeAux(Nodes, ElemToRemove, Acc, Result)
removeAux([], _, Acc, Acc) :- !. %pune acumulatorul in rezultat
removeAux([[N, _] | T], Elem, Acc, Result) :- N = Elem, NewAcc = Acc, removeAux(T, Elem, NewAcc, Result), !.
removeAux([[N, P] | T], Elem, Acc, Result) :- NewAcc = [[N, P] | Acc],  removeAux(T, Elem, NewAcc, Result).

%Intoarce vecinii unui nod intr-o lista de forma [[nod, cost]]
%getNeighbours(Edges, Node, Result)
getNeighbours(Edges, Node, Result) :- getNeighboursAux(Edges, Node, [], Result).

%Functie auxiliara tail recursive.
%getNeighboursAux(Edges, Node, Acc, Result).
getNeighboursAux([], _, Acc, Acc). % pune rezultatul in acumulator
getNeighboursAux([[Node1, Node2, Cost] | T], Node, Acc, R) :- 
		Node1 = Node,  %capatul din stanga al muchiei este identic cu nodul cautat
		NewAcc = [[Node2, Cost] | Acc], %adauga nodul din dreapta si costul ca vecin al nodului
		getNeighboursAux(T, Node, NewAcc, R), !.
getNeighboursAux([[Node1, Node2, Cost] | T], Node, Acc, R) :- 
		Node2 = Node,  %capatul din stanga al muchiei este identic cu nodul cautat
		NewAcc = [[Node1, Cost] | Acc],  %adauga nodul din stanga si costul ca vecin al nodului
		getNeighboursAux(T, Node, NewAcc, R), !.
getNeighboursAux([[_, _, _] | T], Node, Acc, R) :- getNeighboursAux(T, Node, Acc, R), !.

%Gaseste prioritatea nodului Target in lista de noduri si o intoarce in Priority
%getPriority(Nodes, TargetNode, Priority).
getPriority([[N, P] | _], Target, Priority) :- N = Target, Priority = P, !. %am gasit nodul
getPriority([[N, _] | T], Target, Priority) :- N \= Target, getPriority(T, Target, Priority). %continua cautarea

%Parcurge toti vecinii unui nod si verifica daca poate sa gaseasca un drum mai bun printr-un vecin 
%decat cel deja calculat, sau un drum de cost egal, dar printr-un nod cu o prioritate mai mica.
%loopNeighbours(OriginalNodesList, Neighbours, Distances, Previous, Node, MinDistance, 
%				DistancesVectorAcc, PreviousVectorAcc, DistancesResult, PreviousResult)
%Predicatul va genera un nou vector de distante si un vector de parinti, in functie de cum a actualizat costurile drumurilor
loopNeighbours(_, [], _, _, _, _, NewDistances, NewPrevious, OutDistances, OutPrevious) :- 
			OutDistances = NewDistances, OutPrevious = NewPrevious, !. %muta rezultatele din acumulator in variabilele de output

%trateaza cazul cand gasim un drum de cost mai mic
loopNeighbours(OriginalNodes, [[V, Cost]|T], Distances, Previous, Node, MinDistance, _, _, OutDistances, OutPrevious) :- 
	NewCost is MinDistance + Cost, % noul cost daca o luam prin vecinul V al lui Node
	getElem(Distances, V, VDistance), % costul deja cunoscut pentru a ajunge in V
	NewCost < VDistance, % avem un cost mai bun?
	setElem(Distances, V, NewCost, NewDistancesAux), % actualizeaza costul
	setElem(Previous, V, Node, NewPreviousAux), % actualizeaza anteriorul lui V
    loopNeighbours(OriginalNodes, T, NewDistancesAux, NewPreviousAux, Node, MinDistance, NewDistancesAux, NewPreviousAux, 
					OutDistances, OutPrevious), !.

%trateaza cazul cand gasim un drum de acelasi cost, printr-un nod cu o prioritate mai mica
loopNeighbours(OriginalNodes, [[V, Cost]|T], Distances, Previous, Node, MinDistance, _, _, OutDistances, OutPrevious) :- 
	NewCost is MinDistance + Cost,
	getElem(Distances, V, VDistance),
	NewCost = VDistance, %costuri egale
	getPriority(OriginalNodes, Node, NodeP), %prioritatea nodului Node
	getElem(Previous, V, PreviousV), %determina parintele existent al nodului V
	getPriority(OriginalNodes, PreviousV, PreviousVPriority), %obtine prioritatea parintelui lui V
	NodeP < PreviousVPriority, %Node are o prioritate mai mica decat nodul deja salvat in lista de parinti?
	setElem(Distances, V, NewCost, NewDistancesAux),
	setElem(Previous, V, Node, NewPreviousAux),
    loopNeighbours(OriginalNodes, T, NewDistancesAux, NewPreviousAux, Node, MinDistance, NewDistancesAux, NewPreviousAux, OutDistances, OutPrevious), !.

%trateaza cazul cand gasim un drum mai prost, trebuie sa trecem mai departe
loopNeighbours(OriginalNodes, [[_, _]|T], Distances, Previous, Node, MinDistance, _, _, OutDistances, OutPrevious) :- 
    loopNeighbours(OriginalNodes, T, Distances, Previous, Node, MinDistance, Distances, Previous, OutDistances, OutPrevious), !.

%Trebuie sa parcurgem fiecare nod si pentru el sa incercam sa imbunatatim drumurile prin vecinii lui
%whileNodesNotEmpty(OriginalNodes, Nodes, Edges, DistancesVec, PreviousVec, NewDistancesVec, NewPreviousVec)
%OriginalNodes este lista de noduri originala, care nu va fi modificata.
%Este nevoie de ea pentru a putea compara prioritatile, deoarece lista de noduri Nodes o sa fie modificata,
%prin stergerea nodurilor ce au fost deja analizate
whileNodesNotEmpty(_, [], _,  Distances, Previous, Out1, Out2) :- 
		Out1 = Distances, Out2 = Previous, !. %muta rezultatele din acumulator in variabilele de output

whileNodesNotEmpty(OriginalNodes, Nodes, Edges, Distances, Previous, Out1, Out2) :- 
		getNodeWithMinDistance(Nodes, Distances, Node, MinDistance), %ia nodul cu distanta minima dintre nodurile aflate in lista
		remove(Nodes, Node, NewNodes), %sterge nodul din lista
		getNeighbours(Edges, Node, Neighbours), %ia vecinii nodului cu distanta minima
		loopNeighbours(OriginalNodes, Neighbours, Distances, Previous, Node, MinDistance, _, _, OutDistances, OutPrevious), %parcurge vecinii
        whileNodesNotEmpty(OriginalNodes, NewNodes, Edges, OutDistances, OutPrevious, Out1, Out2), !. %treci la urmatorul nod cu dist minima

%Foloseste algoritmul lui dijkstra pentru a determina drumurile minime si costurile lor de la nodul Source la toate celelalte noduri
dijkstra(Nodes, Edges, Source, DistancesVec, PreviousVec) :- 
		setStartVectors(Nodes, Dist, Previous), %initializeaza vectorul de distante si vectorul de parinti
		setElem(Dist, Source, 0, DistResult), %seteaza distanta pt Sursa la 0
		OriginalNodes = Nodes, %retine lista initiala de noduri
		whileNodesNotEmpty(OriginalNodes, Nodes, Edges, DistResult, Previous, DistancesVec, PreviousVec).
 
%Pornind de la vectorul de parinti, construieste drumul intre nodul Source si nodul Destination
%Drumul o sa fie o lista de forma [[Source, Node1], .... [Nodek, Destination]], salvat in PathResult.
%buildPath(Source, Destination, PreviousVec, PathResult).
buildPath(Source, Destination, PreviousVec, PathResult) :- buildPathAux(Source, Destination, PreviousVec, [], PathResult), !.

%Construieste drumul in acumulator si apoi il pune in PathResult.
buildPathAux(Source, Source, _, Acc, Acc) :- !.
buildPathAux(Source, Destination, PreviousVec, Acc, PathResult) :-
		getElem(PreviousVec, Destination, ParentNode), %gaseste nodul anterior nodului destinatie
		AccAux = [[ParentNode, Destination] | Acc], % adauga muchia la drum
		buildPathAux(Source, ParentNode, PreviousVec, AccAux, PathResult), !. %construieste recursiv drumul pana la noul nod destinatie

%Verifica daca un drum contine deja o muchie de la Nodul N1 la nodul N2
%containsEdge(N1, N2, Path, Result).
containsEdge(_, _, [], R) :- R = 0, !.
containsEdge(N1, N2, [[R1, R2] | _], R) :- N1 = R1, N2 = R2, R = 1, !.
containsEdge(N1, N2, [[_, _] | T], R) :- containsEdge(N1, N2, T, R).

%Adauga muchiile drumului Path care nu se gasesc deja in drumul final(FinalPath) - nu vrem duplicate.
%addEdges(Path, FinalPath, Result).
addEdges([], FinalEdges, Answer) :- Answer = FinalEdges, ! .
addEdges([[N1, N2] | T], FinalEdges, Answer) :- 
	containsEdge(N1, N2, FinalEdges, R), 
	R = 0,  % muchia nu exista deja, o adaugam
	append(FinalEdges, [[N1, N2]], OutputEdges), 
	addEdges(T, OutputEdges, Answer), !.
addEdges([[_, _] | T], FinalEdges, Answer) :- addEdges(T, FinalEdges, Answer). %muchia exista deja, trecem la urmatoarea

%Itereaza prin toate nodurile din arbore si construieste arborele de acoperire, adaugat drumurile de la Root la fiecare nod.
%loopAllNodes(Edges, Root, PreviousVector, FinalEdges).
%FinalEdges va contine muchiile arborelui de acoperire in care drumul de la Root la fiecare nod este minim.
loopAllNodes(Edges, Root, PreviousVec, FinalEdges) :- loopAllNodesAux(Edges, Root, PreviousVec, [], FinalEdges).

loopAllNodesAux([], _, _, FinalEdges, FinalEdges) :- !. %muta rezultatul in acumulator
loopAllNodesAux([[N, _] | T], Root, PreviousVec, FinalEdges, Answer) :-	
		N \= Root,  
		buildPath(Root, N, PreviousVec, Path), %genereaza un drum din vectorul de parinti
		addEdges(Path, FinalEdges, NewFinalEdges), %adauga muchiile drumului la arborele final, daca nu exista deja
		loopAllNodesAux(T, Root, PreviousVec, NewFinalEdges, Answer), !. %treci la urmatorul nod din graf
loopAllNodesAux([[N, _] | T], Root, PreviousVec, FinalEdges, Answer) :- 
		N == Root,  % nu avem drum de la radacina la radacina
		loopAllNodesAux(T, Root, PreviousVec, FinalEdges, Answer), !.

%Functiil de mai sus presupun ca nodurile grafului initial sunt consecutive, incepand de la 1, ceea ce nu este cazul in teste(fata de
%problemele de la PA unde nodurile sunt mereu de la 1 la N si este foarte usor de lucrat cu ele).
%Pentru a nu schimba toata implementarea, asociem fiecarui nod din graful initial un numar, incepand de la 1.
%buildNodesMap(OriginalNodesList, MapResult, NewNodesList).
%MapResult reprezinta o lista de perechi de forma (OldNode, NewNode), NewNodesList este efectiv noua lista de noduri
buildNodesMap([], _, [], []) :- !.
buildNodesMap([[N, P] | T], StartNode, MapResult, NewNodeList) :- 
	StartNodeAux is StartNode + 1, %vrem ca nodurile sa inceapa de la 1 si sa fie consecutive
	buildNodesMap(T, StartNodeAux, MapResultAux, NodeAux), 
	MapResult = [[N, StartNode] | MapResultAux],
	NewNodeList = [[StartNode, P] | NodeAux], !.

%Intoarce valoarea asociata(noul numar al nodului) pentru nodul original din graf.
%getValueInNodesMap(NodesMap, OldNodeValue, NewNode).
getValueInNodesMap([[OrigNode, NewNode] | _], ToFind, Result) :- OrigNode = ToFind, Result = NewNode, !.
getValueInNodesMap([[_,_] | T], ToFind, Result) :- getValueInNodesMap(T, ToFind, Result).

%Intoarce valoarea originala a nodului din graf, dandu-se noua valoare
%getValueInNodesMap(NodesMap, NewNodeValue, OldNode).
getValueInNodesMap2([[OrigNode, NewNode] | _], ToFind, Result) :- NewNode = ToFind, Result = OrigNode, !.
getValueInNodesMap2([[_,_] | T], ToFind, Result) :- getValueInNodesMap2(T, ToFind, Result).

%Trebuie sa transformam si muchiile tinand cont de nodurile noi.
%buildEdges(OldEdges, NodesMap, NewEdges).
buildEdges([], _, []) :- !.
buildEdges([[N1, N2, C] | T], NodesMap, NewEdges) :- 
	buildEdges(T, NodesMap, NewEdgesAux), 
	getValueInNodesMap(NodesMap, N1, NewN1),
	getValueInNodesMap(NodesMap, N2, NewN2),
	NewEdges = [[NewN1, NewN2, C] | NewEdgesAux], !.

%Transforma muchiile cu noduri noi in muchiile cu noduri vechi
%buildEdges2(NewEdges, NodesMap, OldEdges).
buildEdges2([], _, []) :- !.
buildEdges2([[N1, N2] | T], NodesMap, NewEdges) :- 
	buildEdges2(T, NodesMap, NewEdgesAux), 
	getValueInNodesMap2(NodesMap, N1, NewN1),
	getValueInNodesMap2(NodesMap, N2, NewN2),
	NewEdges = [[NewN1, NewN2] | NewEdgesAux], !.

%Gaseste nodul root si muchiile arborelui de acoperire
stp([Nodes | [Edges]], Root, FinalEdges) :- 
		minPriority(Nodes, TempRoot), %gaseste nodul cu prioritatea minima.
		Root = TempRoot, %salveaza nodul
		buildNodesMap(Nodes, 1, MapResult, NewNodesList),  %transforma nodurile in noduri consecutive
		buildEdges(Edges, MapResult, NewEdges), %transforma muchiile
		getValueInNodesMap(MapResult, TempRoot, NewRoot), %gaseste noua valoare pentru root
		dijkstra(NewNodesList, NewEdges, NewRoot, _, PreviousVec), %fa dijkstra pe graful simplificat
		%construieste arborele de acoperire, adaugam drumul de la root la fiecare nod.
		loopAllNodes(NewNodesList, NewRoot, PreviousVec, TransformedEges),
		%transforma inapoi muchiile arborelui de acoperire pentru a utiliza vechile noduri.
		buildEdges2(TransformedEges, MapResult, FinalEdges).

%Gaseste nodul root, muchiile arborelui de acoperire si drumul strabatut de un pachet intre nodul Src si nodul Dst
drum([Nodes | [Edges]], Src, Dst, Root, AnswerEdges, AnswerPath) :-
		stp([Nodes | [Edges]], Root, AnswerEdges), %determina rootul si arborele de acoperire
		buildNodesMap(Nodes, 1, MapResult, NewNodesList), %transforma nodurile in noduri consecutive
		%muchiile arborelui nu au informatia de cost, pentru adaugata pentru ca dijkstra sa functioneze
		addCostToEdges(Edges, AnswerEdges, EdgesWithCosts),
		buildEdges(EdgesWithCosts, MapResult, NewEdges), %transforma muchiile pentru a utiliza noile noduri
		getValueInNodesMap(MapResult, Src, NewSrc), %transforma nodul Src
		getValueInNodesMap(MapResult, Dst, NewDst), %transforma nodul Dst
		dijkstra(NewNodesList, NewEdges, NewSrc, _, PreviousVec), %fa dijkstra pe arborele de acoperire
		buildPath(NewSrc, NewDst, PreviousVec, Path), %construieste un drum de la nodul NewSrc la nodul NewDst
		buildEdges2(Path, MapResult, FinalPath), %transforma muchiile pentru a utiliza vechile noduri.
		assemblePath(FinalPath, AnswerPath). %transforma muchiile in noduri [[1,2], [2,3]] -> [1,2,3].

%Tranforma o lista de muchii intr-o lista de noduri
assemblePath(Path, Result) :- assemblePathAux(Path, [], Result).
assemblePathAux([[N1, N2] | []], Acc, Result) :- append(Acc, [N1, N2], Result), !.
assemblePathAux([[N1, _] | T], Acc, Result) :- append(Acc, [N1], NewAcc), assemblePathAux(T, NewAcc, Result), !.

%Adauga cost muchiilor din arborele de acoperire(costul este luat din muchiile grafului de input)
addCostToEdges(OriginalEdges, DijkstraEdges, ResultEdges) :- addCostToEdgesAux(DijkstraEdges, OriginalEdges, [], ResultEdges).

addCostToEdgesAux([], _, Acc, Acc) :- !.
addCostToEdgesAux([[A1, A2] | T], OriginalEdges, Acc, Result) :- 
		findCost(A1, A2, OriginalEdges, C),   %gaseste costul muchiei [A1, A2] din arbore in lista de muchii a grafului
		NewAcc = [[A1, A2, C] | Acc],  %adauga costul
		addCostToEdgesAux(T, OriginalEdges, NewAcc, Result).

%Gaseste costul muchiei data de nodurile [A1, A2] in lista de muchii a grafului si intoarce costul in Cost.
findCost(_, _, [], 0) :- !.
findCost(A1, A2, [[N1, N2, C] | _], Cost) :- A1 = N1,  A2 = N2, Cost = C, !.
findCost(A1, A2, [[N1, N2, C] | _], Cost) :- A1 = N2,  A2 = N1, Cost = C, !.
findCost(A1, A2, [[_, _, _] | T], Cost) :- findCost(A1, A2, T, Cost), !.
