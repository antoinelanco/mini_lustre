# Projet de compilation : MiniLustre

Ceci est un projet pour l'[UE de Marc Pouzet sur les langages réactifs synchrones](https://www.di.ens.fr/~pouzet/cours/comasic/). Le compilateur repose sur l'AST et le parser proposés pour le projet de model checking. L'AST C++ et le compilateur lui-même (qui traduit de Lustre à C++) ont été codés par Antoine Lanco et Jules Pénuchot.

# Approche du compilateur

Plusieurs choix étaient disponibles quant au langage de destination du compilateur : C, Rust ou autre langage bas niveau. Notre choix s'est porté sur C++ qui a des propriétés intéressantes pour modéliser les systèmes réactifs synchrones, tout particulièrement les lambda fonctions, et tout particulièrement les __lambda fonctions mutables__. Cette fonctionnalité du langage permet de créer des lamda fonctions dont l'environnement est modifiable et persistant pour chaque instance, ce qui permet de modéliser facilement les variables locales des noeuds en Lustre.

Un exemple d'implémentation de noeud Lustre en C++ se trouve dans le fichier `examples/ex001_expected.cpp`. Les lambdas se prêtent donc excellement bien à ce jeu car elles permettent d'exprimer de manière __concise__ ce dont nous avons besoin pour modéliser les noeuds Lustre : un __environnement persistant__, grâce aux lambda captures mutables, et un __programme__, car ce sont des fonctions.

Une instance d'un noeud est donc une lambda mutable retournée par une fonction qui la génère. De cette manière nous pouvons nous affranchir d'une lourde contrainte; en effet nous n'avons pas à générer autant de code qu'il y a d'instances de noeuds pour chaque noeud car notre approche ne repose pas sur des variables statiques comme en C.

Une telle implémentation aurait toutefois été possible en C : chaque fonction aurait été associée à un environnement stocké dans un __struct__ passé en argument à chaque appel, ce qui pourrait également être implémenté en C++ grâce aux __objets__, et c'est exactement ce qui est fait par le compilateur lorsqu'on génère une lambda mutable.

Cette approche repose donc sur les abstractions de C++ qui ne sont __pas__ coûteuses par rapport à leur équivalent en C, et permettent de __simplifier__ grandement l'implémentation du compilateur.
