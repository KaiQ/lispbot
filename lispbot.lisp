(defparameter *bot-name* "Lipsy")
(defparameter *server* "irc.freenode.net")
(defparameter *port* 6667)
(defparameter *channel* "#dscc")
(defparameter *commands* (make-hash-table))
(defparameter *SEND-COMMAND* 1)
(defparameter *SEND-CHANNEL* 2)
(defparameter *SEND-PM* 3)
(defparameter *PING-COMMANDS* (make-hash-table))
(defparameter *times* (make-hash-table))
(defparameter *online-users* nil)
(defparameter *laufend* nil)
(defparameter *boese-woerter* '("arsch" "penis" "fotze" "wichser" "noob" "urban" "muma" "boon" "neger" "sucker" "nigger" "fuck" "hure" "schlampe" "bwl" "fick"))
(defparameter *mail* nil)
(defstruct atacke name power)
(defstruct pokemon name lvl ele xp hp hp-max atk def atacken res)
(defparameter *pokemon* (make-hash-table))
(defparameter *std-commands* '(	(?add add) 
				(?clear clear) 
				(?help help) 
				(?version textout "Stabil und gut!")
				(?standard std-commands)
				(?sms sms)
				(?hang hangman)
				(?show show)
				(re textout "wilkommen zurück <from>")
				(^^ textout "*freu*") 
				(nein textout "doch!")
				(? ?42)
				(danke textout "gern geschehen")
				(?bier textout "<to> bekommt ein frisches kühles Jever")
				(?cola textout "<to> bekommt eine frische gekühlte Vita-Cola")
				(warum? textout "darum!")
				(warum textout "weil du es verdienst!")
				(?ghost ghost)
				(?online online)
				(?info info)
				(?w wuerfel)
				(?8ball 8ball)
				(?poke-stats poke-stats)
				(?poke-train poke-train)
				(?poke-attack poke-attack)
				(?poke-choose poke-choose)
				(?poke-center poke-center)
				(?poke-release poke-release)
				(?poke-rename poke-rename)
				(?poke-save poke-save)
				(?poke-show poke-show)
))

(with-open-file (data "pokemon.db" :direction :input) (setf *pokemon* (read data)))
(setf *random-state* (make-random-state t))

(defun std-commands (&rest r)
(mapcar #'(lambda (X) (setf (gethash (intern (string-upcase (car X))) *commands*) (cdr X))) *std-commands*)
(values (format nil "All to default") *SEND-CHANNEL*))

(std-commands)

(defmacro valu (out &body b)
`(values ,@b ,out))

(defun clear (name &optional bef &rest r)
 (values (cond  ((null bef) 
	  (progn
	    (clrhash *commands*)
	    (std-commands)
	    (print "All To Default")))
	((string-equal bef "?CLEAR") (print "?clear kann nicht gelöscht werden"))
	(t (if (gethash (intern (string-upcase bef)) *commands*) 
		(progn (remhash (intern (string-upcase bef)) *commands*) (print (format nil "~A deleted" bef))) 
		"keine solche Funktion gefunden"))) *SEND-CHANNEL*))


(defmacro get-ping-command (name)
`(gethash (if (stringp ,name) (intern ,name) ,name) *ping-commands*))


(defun online (name &rest r)
(values (format nil "~{~A ~}" *online-users*) *Send-PM*))



(defun add-ping-command (name such zeit &optional (com-every t) (com-end t))
(print "add-ping")
(let ((hash (get-ping-command name)))
(if (null hash) (setf (get-ping-command name) (make-hash-table)))
(setf (gethash such (get-ping-command name)) (eval `(let ((counter ,zeit)) (lambda () (cond ((<= counter 0) nil)
					(t (progn (setf counter (- counter 1)) (if (> counter 0) ,com-every (progn (remhash ',such  (get-ping-command ,name)) ,com-end)))))))))))


(defmacro test-ping (name such zeit entweder oder &optional com-every com-end)
(let (	(oder2 `(progn	(add-ping-command ,name ,such ,zeit ,(if (null com-every) t com-every) ,(if (null com-end) t com-end)) 
			,oder)))
`(if	(get-ping-command ,name) 
	(if (gethash ,such (get-ping-command ,name)) ,entweder ,oder2) 
	(progn (setf (get-ping-command ,name) (make-hash-table)) ,oder2))))



(defun help (name &rest r)
(valu *SEND-PM*
(test-ping name 'help 5 "Bitte warten sie nach ein paar Minuten bevor sie ?help erneut aufrufen"
	(list 
      "?add <function-name> <[Platzhalter]output-string>    to add a new function"  
      "?clear [function-name]                               to clear all/one added function[s]"
      "?standard                                            sets all standart functions to its standard"
      "?pm <an> text                                        send personal mail to <an>"
      "?hang <suchwort> [Fehler Anzahl]                     startet hangman mit suchwort und einer anzahl von erlaubten Fehlern"
      "?hang <buchstabe>                                    tippt den buchstaben"
      "?show [function-name]                                prints all function-names or one function-name and their output"
      "?help                                                for this help :-)"
      "?w [zahl]                                            wurf eines <zahl> seitigen wuerfels (standard 6)"
      "?8ball <frage>                                       frage an die magische 8-er Kugel"
      "?poke-choose ?poke-release ?poke-train ?poke-attack ?poke-center ?poke-stats ?poke-show"
      "Platzhalter:"
      "  <FROM>      person die gesendet hat"
      "  <TO>        parameter")
)))

(let((*anz-42* 1)
     (*tiefe* 4))
(defun ?42-help (zahl tiefe)
(cond 	((= 0 tiefe) (progn
(setf *anz-42* (+ 1 *anz-42*))
(if (> *anz-42* 42) (progn (setf *anz-42* 1) (setf *tiefe* (+ *tiefe* 1))))
 (format nil "~D" zahl)))
	(t (format nil "42 + ~A" (?42-help zahl (- tiefe 1))))))

(defun ?42 (name &rest r)
(test-ping name '?42 1 (values "Bitte etwas warten" *send-pm*)(values (?42-help *anz-42* *tiefe*) *SEND-CHANNEL*))))

(defun info (name X &rest r)
(values (cond 	((null X) "Usage: ?info <user>")
		((null (gethash (intern X) *times*)) (format nil "keinen User ~A gefunden" X))
		(t (format nil "letzte aktion von ~A ~A" X (gethash (intern X) *times*)))) *SEND-CHANNEL*))


(defun ghost (name &rest r)
(cond 	((null r) nil)
	(t (values (format nil "~{~A ~}" r) *SEND-CHANNEL*))))



(defun dose (name text)
(format t "DOSE")
(cond 	((null text) nil)
	((null (some #'(lambda (X) (search X text :test 'string-equal)) *boese-woerter*)) nil)
	(t (list (format nil "~{~A~} spendet ~A Euro in die böse wörter dose" name (count-if-not #'null (mapcar #'(lambda (X) (search X text :test 'string-equal)) *boese-woerter*)))))))



(defun 8ball (name &optional &rest r)
(let ((answer (random 20)))
(values (cond	((null r) "ich antworte nur auf fragen")
	((not (equal (aref (car (last r)) (- (length (car (last r))) 1)) #\?)) "ich antworte nur auf Fragen")
	((= 0 answer) "JA, ich bin mir ganz sicher")
	((= 1 answer) "es ist eindeutig JA")
        ((= 2 answer) "ohne jeden Zweifel, JA")
        ((= 3 answer) "JA, definitiv")
        ((= 4 answer) "du solltest darauf vertrauen, dass dem so ist")
        ((= 5 answer) "so wie ich das sehe, JA")
        ((= 6 answer) "vorraussichtlich schon")
        ((= 7 answer) "sieht gut aus")
        ((= 8 answer) "die Zeichen deuten auf JA")
        ((= 9 answer) "JA")
        ((= 10 answer) "die Antwort scheint mir zu verschwommen")
        ((= 11 answer) "frag zu einer fortgeschritteneren Zeit erneut")
        ((= 12 answer) "es ist besser wenn du die Antwort jetzt noch nicht kennst")
        ((= 13 answer) "das kann jetzt noch nicht beantwortet werden")
        ((= 14 answer) "konzentriere dich besser und frage erneut")
        ((= 15 answer) "du solltest nicht darauf hoffen")
        ((= 16 answer) "das sieht nicht danach aus")
        ((= 17 answer) "meine Quellen sagen NEIN")
        ((= 18 answer) "keine guten Aussichten darauf")
        ((= 19 answer) "sehr zweifelhaft")) *SEND-CHANNEL*)))


(defun wuerfel (name &optional (zahl "6")  &rest r)
(let ((z (parse-integer zahl :junk-allowed t)))
(values (cond	((null z) (format nil "~A habt eine ~A gewuerfelt" name (+ 1 (random 6))))
		((< z 1) "nur Zahlen ueber 0 erlaubt")
		(t (format nil "~A hat eine ~A gewuerfelt" name (+ 1 (random z))))) *SEND-CHANNEL*)))


(defun textout (S &optional (from "") (to from) &rest r)
  (if (string-equal to "timeout:") nil)
  (values (replace-string "<TO>" to (replace-string "<FROM>" from S)) *SEND-CHANNEL*))


(defun sms (name &optional (to name) &rest r)
(cond 	((null r) nil)
	((member to *online-users* :test 'string=) (values (format nil "~A from ~A: ~{~A ~}" (now) name r) to))
	((null (placemail to)) (setf *mail* (cons (list to (list (format nil "~A from ~A: ~{~A ~}" (now) name r) to)) *mail*)))
	(t (let ((n (placemail to) )) (setf (nth n *mail*) (append (nth n *mail*) (list (list (format nil "~A from ~A: ~{~A ~}" (now) name r) to))))))))


(defun add (name &optional (a nil) &rest b)
(let ((return (cond ((null b) nil)
		((null a) nil)
		((member a *std-commands* :test #'(lambda (X Y) (equal (string-upcase X) (string-upcase (car Y))))) nil)
		(t (setf (gethash (intern (string-upcase a)) *commands*) (list 'textout (format nil "~{~A ~}" b)))))))
      (values (if (null return) (print "No Function Added") (print "Function Added")) *SEND-CHANNEL*)))

(defun punkte-hangman (spieler)
  (cond ((null spieler) nil)
	(t (cons (format nil "~A mit ~A Punkten" (caar spieler) (cadar spieler)) (punkte-hangman (cdr spieler))))))

(defun setze-wort (buchstabe suchwort ausgabewort)
(print (list buchstabe suchwort ausgabewort))
  (cond ((string= suchwort "") "")
	((string= ausgabewort "") "")
	(t (let ((pos (position buchstabe suchwort :test 'equalp)))
		(cond 	((null pos) (substring ausgabewort (- (length ausgabewort) (length suchwort)))) 
			(t (format nil "~A~A~A" (substring ausgabewort (- (length ausgabewort) (length suchwort)) (+ (- (length ausgabewort) (length suchwort)) pos)) buchstabe (setze-wort buchstabe (substring suchwort (+ pos 1)) ausgabewort))))))))

(defmacro hangman-gewonnen (&body b)
`(progn (setf benutzt nil) (setf spieler (punkte-plus name 10 spieler))  (let ((wort-temp wort)  (punkte (punkte-hangman spieler))) (setf spieler nil) (setf wort "") (cons "Spiel zuende" (cons (format nil "das Wort ist ~A" wort-temp)  punkte))))
)

(defmacro hangman-verloren (&body b)
`(progn (setf benutzt nil) (let ((wort-temp wort)  (punkte (punkte-hangman spieler))) (setf spieler nil) (setf wort "") (cons "Spiel zuende" (cons (format nil "das Wort war ~A" wort-temp)  punkte))))
)


(defmacro hangman-start (&body b)
'(progn (setf wort geraten) (if (parse-integer rounds :junk-allowed t) (setf runden (parse-integer rounds)) (setf runden 10))  (if (< runden (length geraten)) (setf runden (length geraten)))  (setf spieler nil) (setf loesung (coerce (loop repeat (length wort) collect #\*) 'string))  (setf *laufend* t) (format nil "~A hat ein neues Spiel Hangman gestartet (~A Versuche)" name runden) ))


(defun punkte-plus (name punkte liste)
  (cond	((null liste) nil)
	((string= name (caar liste)) (cons (list name (+ punkte (cadar liste))) (cdr liste)))
	(t (cons (car liste) (punkte-plus name punkte (cdr liste))))))


(let ((wort "") (benutzt nil) (spieler nil) (runden 10) (loesung ""))
(defun hangman (name &optional geraten (rounds "10") &rest r)
(if (member name spieler :test #'(lambda (X Y) (equal X (car Y)))) nil (setf spieler (cons (list name 0) spieler)))
(values (cond 	((null geraten) nil)
		((string= wort "") (cond  ((< (length geraten) 5) (list (list "Bitte mehr als 5 Buchstaben" name)))
				  	  ((> (length geraten) 26) (list (list "Bitte nicht mehr als 26 Buchstaben" name)))
					  ((every 'alpha-char-p geraten) (test-ping name 'hang 5 (hangman-start) 
												 (hangman-start) 
												 ;'(if (null *laufend*) t 
												;			(progn (setf *laufend* nil)
												;			       (setf counter 4) t))
												 ;'(if (null *laufend*) (hangman-timeout) t)
									))
					  (t (format nil "~A ist nicht erlaubt" geraten))))
		((< runden (length benutzt)) (hangman-timeout))
		(t (let ((buchstabe (char geraten 0)))
		     (cond ((> (length geraten) 1) (cond 	((string-equal wort geraten) (hangman-gewonnen))
                                                        	(t (progn (setf spieler (punkte-plus name (* -1 (length geraten)) spieler)) 
									  (setf runden (+ 1 runden)) (format nil "~A ist nicht das gesuchte Wort" geraten)))))
		           ((member buchstabe benutzt :test 'equalp) (progn (setf spieler (punkte-plus name -1 spieler))  
			        					    (format nil "~A wurde schon getippt, ~A sollte besser aufpassen!" buchstabe name)))
			   ((find buchstabe wort :test 'equalp) (progn	(setf benutzt (cons buchstabe benutzt)) 
									(setf spieler (punkte-plus name (count buchstabe wort :test 'equalp) spieler))   
									(setf loesung (setze-wort buchstabe wort loesung))  
									(setf runden (+ 1 runden))   
									(if (string-equal wort loesung) (hangman-gewonnen) 
													(list (format nil "~A ~A mal enthalten" buchstabe 
														(count buchstabe wort :test 'equalp)) 
													       loesung))))
			(t (progn (setf benutzt (cons buchstabe benutzt)) (setf spieler (punkte-plus name -1 spieler)) (format nil "~A ist leider nicht enthalten" buchstabe))))))) *SEND-CHANNEL*))


(defun hangman-timeout () 
(cond 	((string= wort "") nil)
	(t (car (mapcar #'(lambda (X) (list X *SEND-CHANNEL*)) (hangman-verloren))))))

)


(defun cut-server (adr)
(subseq adr 0 (search "/" adr)))


(defun cut-http (adr)
(if (null (search "http://" adr)) adr (subseq adr 7)))


(defun add-http (adr)
(if (null (search "http://" adr)) (string-concat "http://" adr) adr))


(defun http-get (adr)
(if (null adr) nil
(ignore-errors 
(apply 'string-concat
(with-open-stream (data (socket-connect 80 
			  (cut-server (cut-http adr))
			  :external-format (ext:make-encoding :charset 
						'charset:iso-8859-1 :line-terminator :UNIX))) 
  (format data "GET ~A HTTP/1.0~%Host: ~A~2%" (add-http adr) (cut-server (cut-http adr)))
  (loop for line = (read-line data nil nil)
      while line
	collect line))))))


(defun parse-title (http)
  (cond ((null (search "<title>" http)) nil)
	((null (search "</title>" http)) nil)
	(t (subseq http (+ (search "<title>" http) 7) (search "</title>" http)))))


(defun link? (adr)
(format t "Link ~A" adr)
(cond ((null adr) nil)
      ((search "http://" adr) adr)
      ((search "www." adr) adr)
      ((search ".de/" adr) adr)
      ((search ".net/" adr) adr)
      ((search ".com/" adr) adr)
      (t nil)))



;(defstruct atacke name power)
;(defstruct pokemon name lvl ele hp hp-max atk def atacken res)
;*pokemons* ((spieler1 pokemon) (spieler2 pokemon)) 

(let ((names '("Glumanda" "Bisasam" "Schiggy" "Pikachu" "Kleinstein" "Zubat" "Mauzi")))
(defun name-poke ()
(nth (random (length names)) names)))

(defun poke-rename (name to &rest r)
(valu *SEND-PM* (cond 	((null to) "Bitte einen Namen angeben")
			(t (progn (setf (pokemon-name (gethash (intern name) *pokemon*)) to) (format nil "Name nach ~A geändert" to))))))

(defun element-poke ()
(random 6))

(defun rand-atk-poke ()
(list (random 10)))

(defun level-up (poke)
(incf (pokemon-lvl poke))
(setf (pokemon-hp-max poke) (+ (pokemon-hp-max poke) (* (pokemon-lvl poke) (+ 1 (random 2)))))
(setf (pokemon-hp poke) (pokemon-hp-max poke))
(setf (pokemon-xp poke) 0)
(setf (pokemon-atk poke) (+ (pokemon-atk poke) (+ 1 (random 10))))
(setf (pokemon-def poke) (+ (pokemon-def poke) (+ 1 (random 10))))
(format nil "~A ist nun Level ~A" (pokemon-name poke) (pokemon-lvl poke))
)


(defun poke-save (name &rest r)
  (with-open-file (data "pokemon.db" :direction :output :if-exists :supersede) 
    (maphash #'(lambda (key val) (if (pokemon-res val) (setf (pokemon-res val) nil))
				(setf (pokemon-hp val) (pokemon-hp-max val))) *pokemon*)
	(print *pokemon* data)))


(defun create-poke (&optional (name "Inkognito") &key (lvl 1) (ele (element-poke)) (xp 0) (hp-max (* 20 lvl)) (atk (* lvl (+ 1 (random 5)))) (def (* lvl (+ 1 (random 5)))) (atacken (rand-atk-poke)) (res nil))
(make-pokemon :name name :lvl lvl :ele ele :xp xp :hp hp-max :hp-max hp-max :atk atk :def def :atacken atacken :res res))


(defun element-win-poke (ele1 ele2)
  (cond ((or (and (evenp ele1)(evenp ele2)) (and (oddp ele1) (oddp ele2))) 2)
	(t 1)))


(defun fight-poke (poke1 name1 poke2 name2)
  (cond ((<= (pokemon-hp poke1) 0) (progn (setf (pokemon-hp poke1) 0) (list (format nil "~A von ~A hat gewonnen" (pokemon-name poke2) name2) (add-xp-poke poke2 (max 0 (- (+ 1 (pokemon-lvl poke1)) (pokemon-lvl poke2)))))))
	(t (progn (setf (pokemon-hp poke2) (- (pokemon-hp poke2) (max 1 (* (pokemon-lvl poke1) (* (element-win-poke (pokemon-ele poke1) (pokemon-ele poke2)) (min (- (pokemon-atk poke1) (pokemon-def poke2)) 1)))))) (fight-poke poke2 name2 poke1 name1)))))


(defun get-pokemon (spieler spielerListe)
  (cond ((null spielerListe) nil)
	((string= (caar spielerListe) spieler) (cadar SpielerListe))
	(t (get-pokemon spieler (cdr spielerListe)))))


(defun get-atacke (nummer)
(nth nummer '("Härtner" "Tackle" "Rutenschlag" "Biss" "Konfusion" "Ruckzuckhieb" "Schaufler" "Donnerschock" "Surfer" "Blubbstrahl")))

(defun add-xp-poke (poke anz)
(if (null poke) nil 
  (let
	((xp (pokemon-xp poke))
	(lvl (pokemon-lvl poke)))
  (cond ((null xp) nil)
	((> xp (* 2 (pokemon-lvl poke))) (level-up poke))
	(t (progn (setf (pokemon-xp poke) (+ xp anz)) nil))))))

(defun poke-stats (name &rest r)
(let ((poke (gethash (intern name) *pokemon*)))
(values (cond ((null poke) "Sie besitzen noch kein Pokemon")
	(t (format nil "~A Level:~A XP:~A  HP:~A/~A Attack:~A Defense:~A Attacken:~{~A ~}" (pokemon-name poke) (pokemon-lvl poke) (pokemon-xp poke) (pokemon-hp poke) (pokemon-hp-max poke) (pokemon-atk poke) (pokemon-def poke) (mapcar #'get-atacke (pokemon-atacken poke))))) *SEND-PM*)))

(defun poke-train (name &optional (numb "")  &rest r)
(values 
  (let ((poke (gethash (intern name) *pokemon*))
	(lvl (parse-integer numb :junk-allowed t)))
	(cond ((null poke) "Sie haben kein Pokemon")
		((pokemon-res poke) "Ihr Pokemon ist noch im Poke-Center")
		((null lvl) (fight-poke poke name (create-poke (name-poke) :lvl (pokemon-lvl poke)) "WILD"))
		(t (fight-poke poke name (create-poke (name-poke) :lvl lvl) "WILD")))) *SEND-PM*))

(defun poke-attack (name &optional (name2 nil) &rest r)
  (cond ((null name2) (valu *SEND-PM* "bitte waehle einen Gegner"))
	(t (let ((poke1 (gethash (intern name) *pokemon*)) (poke2 (gethash (intern name2) *pokemon*)))
		  (cond  ((null poke1) (valu *SEND-PM* "Sie haben kein Pokemon"))
			((null poke2) (valu *SEND-PM* (format nil "~A hat kein Pokemon" name2)))
			((pokemon-res poke1) (valu *SEND-PM* "Ihr Pokemon ist noch im Poke-Center"))
			((pokemon-res poke2) (valu *SEND-PM* (format nil "das Pokemon von ~A ist noch im Poke-Center" name2)))
			((> (* 50 (/ (pokemon-hp poke1) 100))  (pokemon-hp poke2)) (valu *SEND-PM* (format nil "das Pokemon von ~A ist noch zu geschwaecht" name2)))
			(t (valu *SEND-CHANNEL* (let* ((sieger-s (fight-poke poke1 name poke2 name2)) (sieger (caddr (cut (car sieger-s))))) (cons (format nil "~A hat ~A in einem Kampf besiegt" sieger (if (equal sieger name) name2 name )) (cdr sieger-s)) ))))))))

(defun poke-release (name &rest r)
(let ((poke (gethash (intern name) *pokemon*)))
(values 
(cond 	((null poke) "Sie haben kein Pokemon zum freilassen")
	((pokemon-res poke) "Ihr Pokemon ist noch im Poke-Center")
	(t (progn (remhash (intern name) *pokemon*) (format nil "~A wurde frei gelassen" (pokemon-name poke))))) *SEND-PM* )))


(defmacro give-poke (name) 
`(progn (setf (gethash (intern ,name) *pokemon*) (create-pokemon :name ,name )) (format nil "Sie erhalten ein ~A" (pokemon-name (gethash (intern ,name) *pokemon*)))))


(defun poke-choose (name &optional (num "junk") &rest r)
(let ((poke (gethash (intern name) *pokemon*)) (wahl (parse-integer num :junk-allowed t)))
  (values (cond ((null poke) (cond ((null wahl) "Hallo Neuling, sie haben die Wahl zwischen Glumanda(1) Bisasam(2) und Schiggy(3)")
			   ((or (> wahl 3) (< wahl 1)) (progn (setf (gethash (intern name) *pokemon*) (create-poke "Pikachu" :hp-max 40)) (format nil "Sie erhalten ein ~A" (pokemon-name (gethash (intern name) *pokemon*)))))
			   ((= wahl 1) (progn (setf (gethash (intern name) *pokemon*) (create-poke "Glumanda" :hp-max 40)) (format nil "Sie erhalten ein ~A" (pokemon-name (gethash (intern name) *pokemon*)))))
			   ((= wahl 2) (progn (setf (gethash (intern name) *pokemon*) (create-poke "Bisasam" :hp-max 40)) (format nil "Sie erhalten ein ~A" (pokemon-name (gethash (intern name) *pokemon*)))))
			   ((= wahl 3) (progn (setf (gethash (intern name) *pokemon*) (create-poke "Schiggy" :hp-max 40)) (format nil "Sie erhalten ein ~A" (pokemon-name (gethash (intern name) *pokemon*)))))))
	(t "Sie haben schon ein Pokemon")) *SEND-PM*)))


(defun poke-center (name &rest r)
(let ((poke (gethash (intern name) *pokemon*)))
(values (cond ((null poke) "Sie besitzen kein Pokemon")
	(t (test-ping name 'center 1 
		"Ihr Pokemon ist noch im Poke-Center"
		(progn	(setf (pokemon-res poke) t)
			"Ihr Pokemon sollte sich jetzt etwas ausruhen, Ich benachrichtige sie wenn sie es abholen koennen")
		nil
		`(progn	(setf (pokemon-res (gethash (intern ,name) *pokemon*)) nil)
				(setf (pokemon-hp (gethash (intern ,name) *pokemon*)) (pokemon-hp-max (gethash (intern ,name) *pokemon*)))
				(list "Ihr Pokemon hat jetzt wieder volle HP" ,name )))))
	 *SEND-PM*)))


(defun poke-show (name &rest r)
(values (loop for key being the hash-keys of *pokemon* using (hash-value val) collect (format nil "~A hat ein ~A Level ~A" key (pokemon-name val) (pokemon-lvl val))) *SEND-PM*))




(defun liste (A)
(cond 	((null A) A)
	((consp A) A) 
	(t (list A))))



(defun now () (multiple-value-bind (second minute hour date month year) (get-decoded-time) (format nil "~2,'0d:~2,'0d:~2,'0d , ~2,'0d.~d.~d" hour minute second date month year)))


(defun show (name &rest r)
(values (cond 	((null r) (format nil "~{~A ~}" (loop for key being the hash-keys of *commands* collect key)))
		((gethash (intern (string-upcase (car r))) *commands*) (format nil "~A bewirkt ~{~A~}" (car r) (cdr (gethash (intern (string-upcase (car r))) *commands*))))
		(t (format nil "keinen Befehl ~A gefunden" (car r)))) *SEND-PM*))


(defun cut (S)
  (cond ((string= S "") nil)
	((null (position #\space S)) (list S))
	(t (cons (subseq S 0 (position #\space S)) (cut (subseq S (+ 1 (position #\space S))))))))

(defun replace-string (von nach S)
  (cond ((null (search (string-upcase von) (string-upcase S))) S)
	((null nach) S)
	(t (replace-string von nach (concatenate 'string (substring S 0 (search (string-upcase von) (string-upcase S))) 
							  nach 
							 (substring S (+ (search (string-upcase von) (string-upcase S)) (length von))))))))


(defun send-to (channel name to note)
	(cond 	((null note) nil)
		((null to) nil)
		((consp note) (send-to channel name (cadr note) (car note)))
		((equal to *SEND-CHANNEL*) (format nil "PRIVMSG ~A :~A" channel note))
		((equal to *SEND-COMMAND*) note)
		((equal to *SEND-PM*) (format nil "PRIVMSG ~A :~A" name note))
		(t (format nil "PRIVMSG ~A :~A" to note))))



(defun getmail (name &optional (liste *mail*))
(cond 	((null liste) nil)
	((string= name (caar liste)) (cdar liste))
	(t (getmail name (cdr liste)))))


(defun placemail (name &optional (liste *mail*) (anz 0))
(cond 	((null liste) nil)
	((string= name (caar liste)) anz)
	(t (placemail name (cdr liste) (+ 1 anz)))))


(defun send (out channel name to note)
(mapcar #'(lambda (X) (princ (concatenate 'string (send-to channel name to X) (string #\newline)) out)) note))


(defun get-name (text)
  (cond ((null text) nil)
	(t (subseq text 1 (search "!" text)))))



(defun do-ping (key hash)
(loop for val being the hash-values of hash collect (let ((erg (funcall val))) (if (null erg) (remhash key *ping-commands*) erg))))


(defun handle-ping ()
(apply 'append (loop for key being the hash-keys of *ping-commands* using (hash-value val) collect (do-ping key val))))



(defun eva (line commands irc)
(cond ((null line) (start))
      ((equal (subseq line 0 4) "PING") (values (append (list (format nil "PONG ~A" (subseq line 5))) (remove t (handle-ping))) *SEND-COMMAND*))
      (t (let (	(spam (position #\: line :start 1))
		(action (cadr (cut line))))
	(if (null spam)
		nil
		(let* ((name (list (get-name line)))
		       (chattext (cut (subseq line (+ 1 spam))))
		       (H (gethash (intern (string-upcase (car chattext))) commands)))
			  (if (search "freenode" (car name)) t (setf (gethash (intern (car name)) *times*) (now)))
			  (cond ((null chattext) nil)
				((null H) (values (append (mapcar #'(lambda (X) (if (link? X) (parse-title (http-get X)) nil)) (cut (subseq line (+ 1 spam))))
					  (dose name (subseq line (+ 1 spam)))) *SEND-CHANNEL*))
				(t (eval (append H name (cdr chattext)))))))))))



(defun dial-in (name channel)
  (list (format nil "NICK ~A" name)
	(format nil "USER ~A 0 0 ~A" name name)
	(format nil "JOIN ~A" channel)))



(defun bot (name server port channel commands) 
  (with-open-stream (irc (socket-connect port server :external-format (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :UNIX)))
    (send irc "channel" "name" *SEND-COMMAND* (dial-in name channel))
    (loop for i = (read-line irc) while (not (string-equal "=" (cadddr (cut i)))) do (print (cut i)) finally (setf *online-users* (cut (remove #\@  (subseq i (+ 1 (search ":" i :from-end t)))))))
    (loop 
	    (let* ((input (ignore-errors (read-line irc))) 
		  (action (cadr (cut input))))
		  (cond ((string-equal "KICK" action) (send irc "channel" "name" *SEND-COMMAND* (dial-in name channel)))
			((string-equal "JOIN" action) (progn (setf *online-users* (cons (get-name input) *online-users*))
							     (if (null (placemail (get-name input))) t
								   (progn (send irc channel (get-name input) *SEND-PM* (getmail (get-name input)))
							     		     (setf *mail* (remove (nth (placemail (get-name input)) *mail*) *mail* :test 'equal))))))
			((string-equal "QUIT" action) (setf *online-users* (remove (get-name input) *online-users* :test 'string-equal)))
			(t (multiple-value-bind (node to) (eva input commands irc) (send irc channel (get-name input) to (liste node)))))
		  (print input)))))



(defun start ()
(bot *bot-name* *server* *port* *channel* *commands*))
