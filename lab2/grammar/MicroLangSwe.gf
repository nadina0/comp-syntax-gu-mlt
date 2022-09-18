--# -path=.:../abstract
concrete MicroLangSwe of MicroLang = open MicroResSwe, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    S  = {s : Str} ;
    NP = {s : Case => Str ; n : Number ; g : Gender } ;
    VP = {verb : Verb ; compl : Number => Gender => Str} ;
    AP = Adj ;

    CN = Noun ;
    N = Noun ; 
    V = Verb ;
    V2 = Verb2 ;
    A = Adj ;
    Comp = Adj ;
    Adv = {s : Str} ;
    Det = {s : Gender => Art => Str ; n : Number ; sp : Species ; g : Gender} ;
    Pron = {s : Case => Str ; n : Number ; g : Gender } ;
    Prep = {s : Str; } ;


  lin
-- Phrase
    UttS s = s ;
    UttNP np = {s = np.s ! Nom} ;

-- Sentence
    
  PredVPS np vp = {
    s = np.s ! Nom ++ vp.verb.s ! Pres ++ vp.compl ! np.n ! np.g
  } ;

-- Verb

  UseV v = {
      verb = v ;
      compl = \\num,gen => []
      } ;

   ComplV2 v2 np = {
      verb = v2 ;
      
      compl = table {
        n => table {
          g => np.s ! Acc
        } 
      }
      } ;

  UseComp comp = {
      verb = be_Verb ;  
      compl = table {
          n => table {
            g => comp.s ! n ! Indef ! g
          }
      }
  } ;
  
  AdvVP vp adv = { verb = vp.verb ;
                    compl = \\num,gen => vp.compl ! num ! gen ++ adv.s
                    } ;
    

-- Noun

     DetCN det cn = {
      s = table {
        c => det.s ! cn.g ! cn.a ++ cn.s ! det.n ! det.sp
      } ;
      g = cn.g ;
      n = det.n 
    } ;

    UseN n = n ;

    AdjCN ap cn = {
    s = table {
      n => table {
        sp => ap.s ! n ! sp ! cn.g ++ cn.s ! n ! sp
      }
    } ;
    g = cn.g ;
    a = Article
  } ;


     a_Det = {
      s = table {
      Utr => table {
        Article => "en" ;
        NoArticle => ""
      };
      Neutr => table {
        Article => "ett" ;
        NoArticle => ""
      }
    } ;
    n = Sg ;
    g = Utr ;
    sp = Indef
  } ;

    aPl_Det = {
    s = table {
      Utr => table {
        Article => "en" ;
        NoArticle => ""
      } ;
      Neutr => table {
        Article => "ett" ;
        NoArticle => "" 
      } 
    } ;
    g = Utr ;
    n = Pl ;
    sp = Indef
  } ;

  the_Det = {
    s = table {
      Utr => table {
        Article => "den" ;
        NoArticle => ""
      } ;
      Neutr => table {
        Article => "det" ;
        NoArticle => "" 
      } 
    } ;
    g = Utr ;
    n = Sg ;
    sp = Def
  } ;

  thePl_Det = {
    s = table {
      Utr => table {
        Article => "de" ;
        NoArticle => ""
      } ;
      Neutr => table {
        Article => "de" ;
        NoArticle => "" 
      } 
    } ;
    g = Utr ;
    n = Pl ;
    sp = Def
  } ;



    PositA a = a ;

    CompAP ap = ap ;


    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "i"} ;
    on_Prep = {s = "på"} ;
    with_Prep = {s = "med"} ;

    -- Pronouns

    UsePron p = p ;

    he_Pron = {
      s = table {Nom => "han" ; Acc => "honom"} ;
      a = Agr Sg ;
      g = Utr ;
      n = Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "hon" ; Acc => "henne"} ;
      a = Agr Sg ;
      g = Utr ;
      n = Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "de" ; Acc => "dem"} ;
      a = Agr Pl ;
      g = Utr ;
      n = Sg ;
      } ;


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------


--Nouns--

lin animal_N = mkN "djur" Neutr;
lin apple_N = mkN "äpple" Neutr;
lin baby_N = mkN "bebis" Utr;
lin beer_N = mkN "öl" Utr;
lin bike_N = mkN "cykel" Utr;
lin bird_N = mkN "fågel" Utr;
lin blood_N = mkN "blod" Neutr;
lin boat_N = mkN "båt" Utr;
lin book_N = mkN "bok" "boken" "böcker" "böckerna" Utr ;
lin boy_N = mkN "pojke" Utr;
lin bread_N = mkN "bröd" Neutr;
lin car_N = mkN "bil" Utr;
lin cat_N = mkN "katt" Utr;
lin child_N = mkN "barn" Neutr ;
lin city_N = mkN "stad" "staden" "städer" "städerna" Utr;
lin cloud_N = mkN "moln" Neutr ;
lin computer_N = mkN "dator" Utr;
lin cow_N = mkN "ko" Utr;
lin dog_N = mkN "hund" Utr;
lin fire_N = mkN "eld" Utr;
lin fish_N = mkN "fisk" Utr ;
lin flower_N = mkN "blomma" Utr ;
lin friend_N = mkN "vän" Utr ;
lin girl_N = mkN "flicka" Utr;
lin grammar_N = mkN "grammatik" "grammatiken" "grammatiker" "grammatikerna" Utr;
lin horse_N = mkN "häst" Utr;
lin house_N = mkN "hus" Neutr;
lin language_N = mkN "språk" Neutr;
lin man_N = mkN "man" "mannen" "män" "männen" Utr;
lin milk_N = mkN "mjölk" Utr;
lin music_N = mkN "musik" "musiken" "musiker" "musiken" Utr;
lin river_N = mkN "flod" Utr;
lin sea_N = mkN "hav" Neutr;
lin ship_N = mkN "skepp" Neutr;
lin star_N = mkN "stjärna" Utr;
lin train_N = mkN "tåg" Neutr;
lin tree_N = mkN "träd" Neutr;
lin water_N = mkN "vatten" Neutr ;
lin wine_N = mkN "vin" "vinet" "viner" "vinerna" Neutr;
lin woman_N = mkN "kvinna" Utr ;


--Verbs--


lin break_V2 = mkV2 (mkV "bryta" "bryter" "bröt" "brutit") ;
lin buy_V2 = mkV2 (mkV "köpa") ;
lin drink_V2 = mkV2 (mkV "dricka" "dricker" "drack" "druckit") ;
lin eat_V2 = mkV2 (mkV "äta" "äter" "åt" "ätit") ;
lin find_V2 = mkV2 (mkV "hitta") ;
lin kill_V2 = mkV2 (mkV "döda") ;
lin live_V = mkV "leva";
lin love_V2 = mkV2 (mkV "älska") ;
lin play_V = mkV "leka" ;
lin read_V2 = mkV2 (mkV "läsa") ;
lin run_V = mkV "springa" "springer ""sprang" "sprungit" ;
lin see_V2 = mkV2 (mkV "se" "ser" "såg" "sett") ;
lin go_V = mkV "åka" ;
lin sleep_V = mkV "sova" ;
lin swim_V = mkV "simma";
lin teach_V2 = mkV2 (mkV "lära" "lär" "lärde" "lärt") ;
lin travel_V = mkV "resa" ;
lin understand_V2 = mkV2 (mkV "förstå" "förstår" "förstod" "förstått") ;
lin wait_V2 = mkV2 (mkV "vänta") "på";
lin walk_V = mkV "gå" "går" "gick" "gått" ;
lin jump_V = mkV "hoppa" ;
lin come_V = mkV "komma" ;

--Adjectives--

lin bad_A = mkA "dålig" ;
lin big_A = mkA "stor" ;
lin black_A = mkA "svart" ;
lin blue_A = mkA "blå" "blått" "blåa" "blåa" ;
lin clean_A = mkA "ren" ;
lin clever_A = mkA "duktig" ;
lin cold_A = mkA "kall" ;
lin dirty_A = mkA "smutsig" ;
lin good_A = mkA "god" "gott" "goda" "goda" ;
lin green_A = mkA "grön" ;
lin heavy_A = mkA "tung" ;
lin yellow_A = mkA "gul" ;
lin young_A = mkA "ung" ;
lin white_A = mkA "vit" ;
lin warm_A = mkA "varm" ;
lin small_A = mkA "liten" "litet" "lilla" "små"  ;
lin old_A = mkA "gammal" "gammalt" "gamla" "gamla" ;
lin ready_A = mkA "färdig" ;
lin red_A = mkA "röd" "rött" "röda" "röda" ;
lin new_A = mkA "ny" "nytt" "nya" "nya" ;
lin hot_A = mkA "het" ;

--Adverbs--

lin already_Adv = mkAdv "redan" ;
lin now_Adv = mkAdv "nu" ;


---------------------------
-- Paradigms part ---------
---------------------------
  
  oper
  
  --nouns--

  mkN = overload {
    mkN : Str -> Gender -> Noun --regular nouns
      = \Sg_Indef, gender -> lin N (smartNoun Sg_Indef gender) ;
    mkN : Str -> Str -> Str -> Str -> Gender -> Noun -- irregular nouns
      = \Sg_Indef, Sg_Def, Pl_Indef, Pl_Def, gender -> lin N (mkNoun Sg_Indef Sg_Def Pl_Indef Pl_Def gender) ;
  } ;

  --verbs-- 
  
  mkV = overload {
      mkV : (inf : Str) -> V  -- predictable verbs
        = \s -> lin V (smartVerb s) ;
      mkV : (inf,pres,past,part : Str) -> V  -- irregular verbs
        = \inf,pres,past,part -> lin V (irregVerb inf pres past part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object
      = \s   -> lin V2 (regVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition
      = \s,p -> lin V2 (regVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
  } ;
  
  --adjectives--

  mkA = overload {
      mkA : Str -> Str -> Str -> Str -> A
        = \SgIndefUtr,SgIndefNeutr,SgDef,Pl -> lin A (mkAdj SgIndefUtr SgIndefNeutr SgDef Pl) ;
      mkA : Str -> A
        = \adj -> lin A (smartAdj adj) ;
    } ;


  --adverbs--

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  --prepositions--

  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

  }


--notes: problems with vänta på; problems with a_det;