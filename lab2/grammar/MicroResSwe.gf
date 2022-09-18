resource MicroResSwe = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = Utr | Neutr ;
  Case = Nom | Acc ;
  Species = Def | Indef ;
  VForm = Inf | Pres | Past | Part ;
  Agreement = Agr Number Species Gender ;
  Art = Article | NoArticle ;
  
oper

  -- Nouns
    Noun : Type = { s : Number => Species => Str ; g : Gender ; a : Art} ;

    mkNoun : Str -> Str -> Str -> Str -> Gender -> Noun
      = \SgIndef,SgDef,PlIndef,PlDef,gen -> {
        s = table {
          Sg => table {Indef => SgIndef ; Def => SgDef} ;
          Pl => table {Indef => PlIndef; Def => PlDef}
        } ;
      g = gen ;
      a = NoArticle ;
      
    } ;

    smartNoun : Str -> Gender -> Noun = \SgIndef,gen -> case gen of {
      Utr => case SgIndef of {
        blomm + "a" => mkNoun SgIndef (blomm + "an") (blomm + "or") (blomm + "orna") gen ; -- group 1 nouns: utr ending with an "a"
        pojk + "e" => mkNoun SgIndef (pojk + "en") (pojk + "ar") (pojk + "arna") gen ; -- group 2 nouns: utr ending with an unstressed "e"
        cyk + "el" => mkNoun SgIndef (cyk + "eln") (cyk + "lar") (cyk + "larna") gen ; -- still group 2 but ending with "el"
        k + "o" => mkNoun SgIndef (k + "on") (k + "or") (k + "orna") gen ;        
        ka + "tt" => mkNoun SgIndef (ka + "tten") (ka + "tter") (ka + "tterna") gen ;                
        bil => mkNoun SgIndef (SgIndef + "en") (SgIndef + "ar") (SgIndef + "arna") gen  --still group 2 but just one syllable
      } ;
      Neutr => case SgIndef of {
        äppl + "e" => mkNoun SgIndef (äppl + "et")  (äppl + "en") (äppl + "ena") gen ; --group 4 nouns: neutrum ending with vowel
        hus => mkNoun SgIndef (SgIndef + "et") (SgIndef) (SgIndef + "en") gen --group 5 nouns: neutrum ending with consonant
      }
    } ;



-- Verbs

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,part : Str) -> Verb
    = \inf,pres,past,part -> {
    s = table {
      Inf => inf ;
      Pres => pres ;
      Past => past ;
      Part => part 
      }
    } ;

  --most common verbs
  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "r") (inf + "de") (inf + "t") ;

   --regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
    verb + "a" => case verb of {
      _ + "imm"                   => mkVerb inf (verb + "ar") (verb + "ade") (verb + "at") ;
      _ + "pp"      => mkVerb inf (inf + "r") (inf + "de") (inf + "t") ;
      _ + "sk"                    => mkVerb inf (verb + "ar") (verb + "ade") (verb + "at") ;
      _ + ("s"|"p"|"ek")         => mkVerb inf (verb + "er") (verb + "te") (verb + "t") ;
      _ + ("k"|"yt"|"mm")         => mkVerb inf (verb + "er") (verb + "ade") (verb + "at") ; 
      _ + "ät"                    => mkVerb inf (verb + "er") (inf + "de") (inf + "t") ;
      _ + ("t"|"d") => mkVerb inf (inf + "r") (inf + "de") (inf + "t") ;
      _ + "v"               => mkVerb inf (verb + "er") (verb + "de") (verb + "t") 
    } ;            
     _ => regVerb inf
     } ;
 

  -- irregular verbs
      irregVerb : (inf,pres,past,part : Str) -> Verb =
      \inf,pres,past,part ->
        mkVerb inf pres past part ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "vara" "är" "var" "varit" ;


  --Adjectives

  Adj : Type = {
    s : Number => Species => Gender => Str;
  } ;

  mkAdj : Str -> Str -> Str -> Str -> Adj
    = \SgIndefUtr, SgIndefNeutr, SgDef, Pl -> {
      s = table {
        Sg => table {
            Indef => table {Utr => SgIndefUtr ; Neutr => SgIndefNeutr};
            Def => table {x => SgDef}
        } ;
        Pl => table {
            _=> table {
                _ => Pl
            }
          }
      }
  } ;

  smartAdj : Str -> Adj = \adj -> case adj of {
    smar + "t" => mkAdj adj adj (adj + "a") (adj + "a") ;
    _ => mkAdj adj (adj + "t") (adj + "a") (adj + "a")
  } ;
    

}