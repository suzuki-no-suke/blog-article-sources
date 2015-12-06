#|
test using FiveAM
this test should runs on ":cl-user" package

need to load FiveAM before run thisfile.

(asdf:load-system "fiveam")
or : (require 'fiveam)  ;<- it can run on sbcl
(load "factorial.lisp")
(load "factorial-test.lisp")
(5am:run!)

|#

;;; factorial result values with 10, 50, 100, 1000
(defconstant +fact10+ 3628800)
(defconstant +fact50+ 30414093201713378043612608166064768844377641568960512000000000000)
(defconstant +fact100+ 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000)
(defconstant +fact1000+ 402387260077093773543702433923003985719374864210714632543799910429938512398629020592044208486969404800479988610197196058631666872994808558901323829669944590997424504087073759918823627727188732519779505950995276120874975462497043601418278094646496291056393887437886487337119181045825783647849977012476632889835955735432513185323958463075557409114262417474349347553428646576611667797396668820291207379143853719588249808126867838374559731746136085379534524221586593201928090878297308431392844403281231558611036976801357304216168747609675871348312025478589320767169132448426236131412508780208000261683151027341827977704784635868170164365024153691398281264810213092761244896359928705114964975419909342221566832572080821333186116811553615836546984046708975602900950537616475847728421889679646244945160765353408198901385442487984959953319101723355556602139450399736280750137837615307127761926849034352625200015888535147331611702103968175921510907788019393178114194545257223865541461062892187960223838971476088506276862967146674697562911234082439208160153780889893964518263243671616762179168909779911903754031274622289988005195444414282012187361745992642956581746628302955570299024324153181617210465832036786906117260158783520751516284225540265170483304226143974286933061690897968482590125458327168226458066526769958652682272807075781391858178889652208164348344825993266043367660176999612831860788386150279465955131156552036093988180612138558600301435694527224206344631797460594682573103790084024432438465657245014402821885252470935190620929023136493273497565513958720559654228749774011413346962715422845862377387538230483865688976461927383814900140767310446640259899490222221765904339901886018566526485061799702356193897017860040811889729918311021171229845901641921068884387121855646124960798722908519296819372388642614839657382291123125024186649353143970137428531926649875337218940694281434118520158014123344828015051399694290153483077644569099073152433278288269864602789864321139083506217095002597389863554277196742822248757586765752344220207573630569498825087968928162753848863396909959826280956121450994871701244516461260379029309120889086942028510640182154399457156805941872748998094254742173582401063677404595741785160829230135358081840096996372524230560855903700624271243416909004153690105933983835777939410970027753472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

;;; factorial test generate macro
(defmacro 5am-fact-test (test-name func-name)
  `(5am:test ,test-name
     (5am:is (= (,func-name 10) +fact10+))
     (5am:is (= (,func-name 50) +fact50+))
     (5am:is (= (,func-name 100) +fact100+))
     (5am:is (= (,func-name 1000) +fact1000+))))

;;;; ---------------------------------------------------------------------------
;;;; sub function tests
;;;; ---------------------------------------------------------------------------
;;; iota test macro
(5am:test sub-iota
  (5am:is (equal (iota1-n 5) '(1 2 3 4 5)))
  (5am:is (equal (iota1-n 10) '(1 2 3 4 5 6 7 8 9 10)))
  (5am:is (equal (iota1-n 2) '(1 2))))

;;;; ---------------------------------------------------------------------------
;;;; answer check tests
;;;; ---------------------------------------------------------------------------

;; process with numbers, procedual
(5am-fact-test dotimes factorial-dotimes)
(5am-fact-test simple-loop factorial-simple-loop)
(5am-fact-test do factorial-do)
(5am-fact-test do* factorial-do*)
(5am-fact-test recursive factorial-recursive)
(5am-fact-test labels factorial-labels)
(5am-fact-test labels-tail-recursion factorial-labels-tail-recursion)
(5am-fact-test loop factorial-loop)
(5am-fact-test loop-decrement factorial-loop-decrement)
(5am-fact-test tagbody factorial-tagbody)

;; special
(5am-fact-test dolist factorial-dolist)
(5am-fact-test args-optional factorial-args-optional)
(5am-fact-test apply factorial-apply)
(5am-fact-test reduce factorial-reduce)
(5am-fact-test clos-method factorial-clos-method)
(5am-fact-test with-macro factorial-with-macro)
(5am-fact-test with-recursive-macro factorial-with-recursive-macro)
(5am-fact-test with-outside-macro factorial-with-outside-macro)
(5am-fact-test length factorial-length)

;;;; ---------------------------------------------------------------------------
;;;; list process variations - not count as answer
;;;; ---------------------------------------------------------------------------
(5am-fact-test simple-loop-list factorial-simple-loop-list)
(5am-fact-test do-list factorial-do-list)
(5am-fact-test labels-list factorial-labels-list)
(5am-fact-test labels-tail-recursion-list factorial-labels-tail-recursion-list)
(5am-fact-test loop-macro-list factorial-loop-macro-list)
(5am-fact-test tagbody-list factorial-tagbody-list)

;;;; ---------------------------------------------------------------------------
;;;; sample - not count as answer
;;;; ---------------------------------------------------------------------------
(5am-fact-test dotimes-too-redundant factorial-dotimes-too-redundant)
(5am-fact-test dotimes-as-index factorial-dotimes-as-index)
(5am-fact-test dolist-too-redundant1 factorial-dolist-too-redundant1)
(5am-fact-test dolist-too-redundant2 factorial-dolist-too-redundant2)
(5am-fact-test dolist-index-of-item factorial-dolist-index-of-item)
(5am-fact-test simple-loop-variables-on-argument factorial-simple-loop-variables-on-argument)
(5am-fact-test do-let factorial-do-let)
(5am-fact-test do-decrement factorial-do-decrement)
(5am-fact-test do-variables-on-argument factorial-do-variables-on-argument)
(5am-fact-test tail-recursion factorial-tail-recursion)
(5am-fact-test loop-with-let factorial-loop-with-let)
(5am-fact-test macro-dotimes factorial-macro-dotimes)
