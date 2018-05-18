-- UQAM - Hiver 2017 - INF2160 - Groupe 30 - TP1
--
-- miniTex.hs - Code source du programme qui procede au traitement d'un 
-- fichier texte contenant des commandes (exemple : "\section{titre}{id}") 
-- et qui genere le texte dans son format d'affichage desire. 
--
-- Le bon fonctionnement du programme depend de deux conditions :
--
-- 1 - Les caracteres '{', '}' et '\' ne peuvent etre dans le texte a traiter
--     qu'a l'interieur des commandes.
-- 2 - Les commandes respectent la definition de leurs formats. 
--
-- @Auteur : Chretien Alexis (CHRA25049209)
-- @Version : 6 mars 2017
--
import System.Environment(getArgs)

type Type             = String
type Titre            = String
type StrNumero        = String
type Identificateur   = String
type DocumentBrut     = String
type LigneBrute       = String
type Ligne            = [String]
type ListeDeLignes    = [String]
type Document         = [[String]]
type Indexe           = [(String, String, String, String)]
type ListeDelimiteurs = [Char]

-- Fonction qui decoupe une chaine de caracteres (une "ligne brute") en
-- utilisant une liste de caracteres delimiteurs. 
--
-- Implantation tiree des exemples du cours.
-- (auteur : Bruno Malenfant
-- www.labunix.uqam.ca/~malenfant_b/inf2160/Indexe.hs)
-- 
-- @param  listeDelimiteurs    La liste de caracteres delimiteurs.
-- @param  LigneBrute          La chaine de caracteres a diviser.
-- @return Ligne               La ligne retournee. 
diviserChaine :: ListeDelimiteurs -> LigneBrute -> Ligne
diviserChaine diviseurs ss =
    case dropWhile ((flip elem) diviseurs) ss of
     "" -> []
     ss' -> e : diviserChaine diviseurs ss''
         where (e, ss'') = break  ((flip elem) diviseurs) ss'

-- Fonction qui appelle diviserChaine en utilisant le caractere de fin de ligne
-- comme caractere separateur. Divise une chaine de caracteres en une liste 
-- de chaines de caracteres, ou chaque element de la liste represente une ligne
-- du document.
--
-- @param  DocumentBrut     La chaine de caracteres a traiter (le document dans
--                          son format original).
-- @return ListeDeLignes    La liste de chaines de caracteres proprement divisee 
--                          selon le caractere de fin de ligne '\n'.
diviserLignes :: DocumentBrut -> ListeDeLignes
diviserLignes = diviserChaine ['\n']

-- Fonction qui appelle diviserChaine en utilisant les caracteres '{', '}' '\'
-- sur le document divise en lignes.  
--
-- @param   ListeDeLignes    Le document a traiter, qui a ete divise en une liste 
--                           chaines de caracteres selon le caractere de fin de ligne.
-- @return  Document         Le document sous forme de liste de liste de chaines de 
--                           caracteres. Chaque element des sous-listes est soit 
--                           le nom d'une commande, un argument de commande ou une
--                           section de texte qui n'a pas a etre traite. 
diviserCommandes :: ListeDeLignes -> Document
diviserCommandes [] = []
diviserCommandes (ligne:resteDoc) = (diviserChaine ['{','}', '\\'] ligne) : (diviserCommandes resteDoc)


-- Fonction qui enleve des espaces du document sous forme de liste de liste de
-- chaines de caracteres.
--
-- Le fait de diviser le document selon les caracteres '{', '}' et '\' laisse
-- des caracteres "espace" en debut et/ou en fin des chaines de caracteres 
-- constituant la liste de listes de chaines de caracteres, ou cree des chaines
-- qui ne sont que la chaine " ". Cette fonction enleve ces espaces pour 
-- simplifier le reformattage final du document. 
--
-- @param   Document        Le document a traiter, contenant des espaces indesires.
-- @return  Document        Le document sans espaces indesires.
enleverEspaces :: Document -> Document
enleverEspaces [] = []
enleverEspaces (ligne:resteDoc) =  (enleverEspaces' (filter (/= " ") ligne)):(enleverEspaces resteDoc)
enleverEspaces' [] = []
enleverEspaces' (mot:resteLigne) | (head mot) == ' ' = enleverEspaces' ((tail mot):resteLigne)
                                 | (last mot) == ' ' = enleverEspaces' ((init mot):resteLigne)
                                 | otherwise         = mot       :(enleverEspaces' resteLigne)

-- Fonction qui genere un indexe a partir du document contenant les commandes.
--
-- L'indexe est une liste de quadruplets contenant le type de la commande
-- ("Section", "Figure" ou "Table"), son identificateur, son titre et son
-- numero (sous forme de chaine de caracteres).
--
-- @param   Document        Le document pour lequel on veut generer un indexe.
-- @return  Indexe          L'indexe du document qui conserve les informations
--                          sur les sections, les tables et les figures.
creerIndexe :: Document -> Indexe
creerIndexe [] = []
creerIndexe document = creerIndexe' 0 0 0 document
creerIndexe' _ _ _ [] = []
creerIndexe' noSec _     _     (("section":titre:id:resteLigne):resteDoc) = ("Section", id, titre, show (noSec + 1)):(creerIndexe' (noSec + 1) 0 0 (resteLigne:resteDoc))
creerIndexe' noSec noFig noTab (("figure" :titre:id:resteLigne):resteDoc) = ("Figure",  id, titre, (show noSec) ++ "." ++ (show (noFig + 1))) : (creerIndexe' noSec (noFig + 1) noTab (resteLigne:resteDoc))
creerIndexe' noSec noFig noTab (("table"  :titre:id:resteLigne):resteDoc) = ("Table",   id, titre, (show noSec) ++ "." ++ (show (noTab + 1))) : (creerIndexe' noSec noFig (noTab + 1) (resteLigne:resteDoc))
creerIndexe' noSec noFig noTab ((mot:resteLigne):resteDoc) = creerIndexe' noSec noFig noTab (resteLigne:resteDoc)
creerIndexe' noSec noFig noTab ([]:resteDoc) = creerIndexe' noSec noFig noTab resteDoc   

-- Fonction qui renvoie le type d'un element du texte (Section, Figure ou Table)
-- en cherchant selon l'identificateur.
--
-- @param   Indexe          L'indexe des sections, tables et figures.
-- @param   Identificateur  L'identificateur de l'element cherche.
-- @return  Type            Le type de l'element, sous forme de chaine
--                          de caracteres.
trouverType  :: Indexe -> Identificateur -> Type
trouverType [] _ = ""
trouverType ((leType, id,_,_):resteIndexe) idDesire | id == idDesire = leType
                                                    | otherwise = trouverType resteIndexe idDesire

-- Fonction qui renvoie le titre d'un element du texte (Section, Figure ou Table)
-- en cherchant selon l'identificateur.
--
-- @param   Indexe          L'indexe des sections, tables et figures.
-- @param   Identificateur  L'identificateur de l'element cherche.
-- @return  Type            Le type de l'element, sous forme de chaine
--                          de caracteres.
trouverTitre :: Indexe -> Identificateur -> Titre
trouverTitre [] _ = ""
trouverTitre ((_, id, titre,_):resteIndexe) idDesire | id == idDesire = titre
                                                     | otherwise      = trouverTitre resteIndexe idDesire 

-- Fonction qui renvoie le numero d'un element du texte (Section, Figure ou Table)
-- en cherchant selon l'identificateur
--
-- @param   Indexe          L'indexe des sections, tables et figures.
-- @param   Identificateur  L'identificateur de l'element cherche.
-- @return  StrNumero       Le numero de l'element, sous forme de chaine
--                          de caracteres.
trouverNumero :: Indexe -> Identificateur -> StrNumero
trouverNumero [] _ = ""
trouverNumero ((_, id,_, numero):resteIndexe) idDesire | id == idDesire = numero
                                                       | otherwise      = trouverNumero resteIndexe idDesire

-- Fonction qui prend une ligne contenant une commande ("section", "figure", "table"
-- ou "ref") et qui renvoie la ligne dans un format conforme a l'affichage desire. 
--
-- @param   Indexe          L'indexe des sections, tables et figures.
-- @param   Ligne           La ligne a traiter.
-- @return  Ligne           La ligne dans son format d'affichage. 
retournerLigneFormattee :: Indexe -> Ligne -> Ligne
retournerLigneFormattee _ [] = []
retournerLigneFormattee indexe ("section":titre:id:resteLigne) = ("Section " ++ (trouverNumero indexe id) ++ " : " ++ titre):(retournerLigneFormattee indexe resteLigne)
retournerLigneFormattee indexe ("figure" :titre:id:resteLigne) = ("Figure "  ++ (trouverNumero indexe id) ++ " : " ++ titre):(retournerLigneFormattee indexe resteLigne)
retournerLigneFormattee indexe ("table":  titre:id:resteLigne) = ("Table "   ++ (trouverNumero indexe id) ++ " : " ++ titre):(retournerLigneFormattee indexe resteLigne)
retournerLigneFormattee indexe ("ref":          id:resteLigne) = ("(voir la " ++ (trouverType indexe id) ++ " " ++ (trouverNumero indexe id) ++ ")"):(retournerLigneFormattee indexe resteLigne) 
retournerLigneFormattee indexe (mot:resteLigne) = mot:(retournerLigneFormattee indexe resteLigne)

-- Fonction qui recherche les lignes contenant une commande et qui remplace ces
-- lignes par les lignes dans leur format d'affichage en faisant appel a 
-- retournerLigneFormattee.
--
-- La fonction genere egalement l'indexe via la fonction creerIndexe, et envoie
-- l'indexe en argument a retournerLigneFormattee. 
--
-- @param   Document        Le document ayant des lignes contenant des commandes.
-- @param   Document        Le document ayant ses lignes remplacees par des
--                          des chaines de caracteres respectant le format 
--                          d'affichage desire.
traiterCommandes :: Document -> Document
traiterCommandes':: Indexe   -> Document -> Document
traiterCommandes [] = []
traiterCommandes document = traiterCommandes' (creerIndexe document) document
traiterCommandes' _ [] = []
traiterCommandes' indexe (ligne:resteDoc) | any (flip elem commandes) ligne = (retournerLigneFormattee indexe ligne):(traiterCommandes' indexe resteDoc)
                                          | otherwise                       = ligne:(traiterCommandes' indexe resteDoc)
                                                where commandes = ["section", "figure", "table", "ref"]

-- Fonction qui transforme le document sous la forme de liste de listes de chaines 
-- de caracteres en une seule chaine de caracteres, en y ajoutant les espaces et 
-- les caracteres de fin de ligne la ou c'est necessaires. 
--
-- @param   Document                Le document a traiter.
-- @return  DocumentBrut            Le document en une seule chaine de caracteres.
reconstruireDocument :: Document -> DocumentBrut
reconstruireDocument [] = ""
reconstruireDocument ([]:resteDocument) = reconstruireDocument resteDocument
reconstruireDocument ([mot] : resteDocument) = mot ++ "\n" ++ (reconstruireDocument resteDocument)
reconstruireDocument ([mot, "."] : resteDocument) = mot ++ ".\n" ++ (reconstruireDocument resteDocument)
reconstruireDocument ((mot : "." : resteLigne) : resteDocument) = mot ++ ". "  ++ (reconstruireDocument (resteLigne:resteDocument))
reconstruireDocument ((mot : resteLigne) : resteDocument) = mot ++ " " ++ (reconstruireDocument (resteLigne:resteDocument))

-- Fonction qui traite un document brut (une chaine de caractere) contenant des
-- commandes "\section", "\table", "\figure" et "\ref" et qui le transforme en une 
-- chaine de caracteres qui satisfait le format desire.       
--
-- @param  DocumentBrut      La chaine de caracteres du fichier a traiter
-- @return Document          La chaine de caracteres traitee.                                                 
miniTex :: DocumentBrut -> DocumentBrut
miniTex  = reconstruireDocument . traiterCommandes . enleverEspaces . diviserCommandes . diviserLignes

-------------
--   Main  --
-------------
main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
