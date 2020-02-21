; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Gonca Ezgi Çakır                 *
; *		     151044054                        *
; *          HW2 PART2                        *
; *********************************************

;REGULAR EXPRESSIONS
; [;;]+([. + - / *]|[A-Za-z]|[0-9]|[ \t])* 			/*comment -> ;; characteer could fallow by sign, alphabetic character or digit*/  
;[0]												/*0 digit */  
;[1-9][0-9]* 										/*invalid integer value -> integeres followed by integer values with leading zeros */  
;[0-9][0-9]* 										/*integer value -> integeres followed by integer values with no leading zeros*/
;[a-zA-Z][_a-zA-Z]* 								/*identifier -> alphabetic character could fallow each other*/
;[ \t\n]+											/*ignored characters -> space, newline, tab*/

;;RESERVED KEYWORDS
(defvar KW_AND "and")
(defvar KW_OR "or")
(defvar KW_NOT "not")
(defvar KW_EQUAL "equal")
(defvar KW_LESS "less")
(defvar KW_NIL "nil")
(defvar KW_LIST "list")
(defvar KW_APPEND "append")
(defvar KW_CONCAT "concat")
(defvar KW_SET "set")
(defvar KW_DEFFUN "deffun")
(defvar KW_FOR "for")
(defvar KW_IF "if")
(defvar KW_EXIT "exit")
(defvar KW_LOAD "load")
(defvar KW_DISP "disp")
(defvar KW_TRUE "true")
(defvar KW_FALSE "false")

;;OPERATORS
(defvar OP_PLUS "+")
(defvar OP_MINUS "-")
(defvar OP_DIV "/")
(defvar OP_MULT "*")
(defvar OP_DBLMULT "**")
(defvar OP_COMMA ",")
(defvar OP_OC "‘")
(defvar OP_CC "’")
(defvar OP_OP "(")
(defvar OP_CP ")")

;;COMMENT
(defvar COMMENT ";;")
(setq SEMICOLON ";")

;;KONTROL VARIABLES
(setq DFA 0)

(setq control 2)
(setq isInteger 0)
(setq isOperator 0)
(setq isKEYorID 0)
(setq isIdentifier 0)
(setq isDblmult 0)
(setq isMult 0)
(setq isComment 0)

(setq token_key_id "")
(setq token_int "")
(setq token_operator "")
(setq token_mult "")
(setq token_comment "")

;;---------------------------------------------------------------------------------------------------------------------

;interpreter function
;read file or terminal commands
;reads first line and stores it in string
;sends line string to 'gpplexer' function
(defun gppinterpreter ()

	(format t "> " ) ;prints terminal symbol

	(setq terCom (read-line) )	;gets terminal command and initializes it	
	(split_terminal_command (coerce terCom 'list) ) ;splits terminal command in order to purpose of usage
)

;lexer function
;gets filename and control variable
;when contol is 1 filename is a string of terminal command
;when control is 0 filename is a file stream
;DFA used to control syntax and terminate program
(defun gpplexer(filename control)
	
	(cond
		;file condition
		((eq control 0)
			(with-open-file (stream filename)
				(do ((chr (read-char stream nil) (read-char stream nil))) ;reads char by char from file
					((or (null chr) (eq DFA 1)))
					
					(gpp_lexer_helper chr stream) ;specifies every chr according to rules, then sends it to lexer
				)	
			)
		)
	
		;terminal condition
		((eq control 1)

			(loop for i from 0 to (- (length filename) 1) ;reads char by char form string "filename"
	    		do 
	    		(if (eq DFA 0)
	        		(progn	
	        			(setq chr (nth i filename))
						(gpp_lexer_helper chr filename) ;specifies every chr according to rules, then sends it to lexer
					)
				)		
			)		
		)	
	)
)

;HELPER FUNCTIONS ------------------------------------------------------------------------------------------------------

;helper function (gppinterpreter)
;gets a list 
;splits list to get file name
;returns file name
(defun split_terminal_command (terCom)

	(setq spaceCount 0) ;space sign counter
	(setq plusCount 0) ;plus sign counter
	(setq get_filename '()) 
	
	(loop for i from 0 to (- (length terCom) 1)
		do	
			(if (eq (nth i terCom) #\Space) 
				(setq spaceCount (+ spaceCount 1)) ;counts space in the terminal command
		    )	

		    (if (eq (nth i terCom) #\+) 
				(setq plusCount (+ plusCount 1)) ;counts plus sign in the terminal command
		    )	
	)

	(cond 
		;invalid input condition  
		( (or (< (length terCom) 5) (eq plusCount 0) (< plusCount 2)) 
		  	(print "Wrong command.Try again.")  ;error has found
		    (terpri)
		    (gppinterpreter) 	;interpreter is executed again
		)

		;lexer executes on terminal condition
		( (and (<= spaceCount 2) (<= (length tercom) 6)) 
			(setq terCom_new (read-line)) ;reads next line
			(setf control 1) 
			(setq terCom_new (coerce terCom_new 'list))
			(gpplexer terCom_new control) ;calls gpplexer function for terminal code
		)	

		;lexer executes on file condition
		( (and (>= spaceCount 2) (> (length tercom) 6)) 
			(setq get_filename (values (subseq tercom 6))) ;gets filename to execute lexer on file 
			(setf control 0) 
			(setq filename (coerce get_filename 'string)) ;converts filename to string
			(gpplexer filename control) ;calls lexer function for file code
		) 
	)
)

;helper function (gpplexer)
;specifies tokens and sets control variables
;gets single character and file stream
(defun gpp_lexer_helper (chr stream)
	(cond
				
		;condition to control chr is integer
		((digit-char-p chr)
			(setf token_int (concatenate 'string token_int (list chr))) ;add chr into token_int string
			(setf isInteger 1)
		)

		;condition to control chr is identifier
		((alpha-char-p chr)
			(setf token_key_id (concatenate 'string token_key_id (list chr))) ;adds chr into token_key_id string
			(setf isKEYorID 1)
			(setf isIdentifier 1)
		)

		;when there is a space,newline or tab condition 
		;then sends token to print_lexer function 
		((or (eq chr #\Space) (eq chr #\Tab) (eq chr #\Newline) (eq chr nil))
			;sets control flags according to length
			(cond 
				((eq (length token_mult) 2) (setf isDblmult 1))
				((eq (length token_mult) 1) (setf isMult 1))
			)

			(cond
				((eq isInteger 1) (print_lexer token_int)) ;calls function to print integer value 
				((eq isKEYorID 1) (print_lexer token_key_id) ) ;calls function to print identifier or reserved keyword
				((or (eq isMult 1) (eq isDblmult 1) ) (print_lexer token_mult))	;calls function to print mult or dblmult operator 	
			)				
		)

		;condition to control chr is operation (except mult and dublmult)
		((or (string-equal chr OP_PLUS) (string-equal chr OP_MINUS) (string-equal chr OP_DIV) 
			 (string-equal chr OP_OP) (string-equal chr OP_CP) (string-equal chr OP_OC) (string-equal chr OP_CC) )
							
			(cond
				((eq isInteger 1) (print_lexer token_int)) ;calls function to print integer value 
				((eq isIdentifier 1) (print_lexer token_key_id)	)	;calls function to print identifier or reserved keyword 
			)		
						
			(setf isOperator 1)	
			(setf token_operator (concatenate 'string token_operator (list chr))) ;add operator to token_operator string
			(print_lexer token_operator) ;calls function to print operator 					

		)

		;condition to control chr is mult or dblmult
		((string-equal chr OP_MULT)
			(setf token_mult (concatenate 'string token_mult (list chr))) ;adds operator to token_mult string
		)

		;condition to control chr is semicolon 
		((string-equal chr SEMICOLON)
			(setf token_comment (concatenate 'string token_comment(list chr))) ;adds chr to token_comment string
			
			(if (eq (length token_comment) 2) ;there is a double semicolon
				(progn 
					(setf isComment 1)
					(print_lexer token_comment) ;;calls function to print comment 
					(setf stream (readfile_for_command stream)) ;calls function to skip a line after command symbol
				)
			)

		)			

	)

)

;helper function (lexer)
;function to skip line after a COMMENT keyword
;gets file stream 
;returns file stream after from newline character
(defun readfile_for_command (stream)
	(loop
		(setf item (read-char stream )) ;reads char by char from file
			
		(when (eq item #\Newline) (return-from readfile_for_command stream)) ;when item is 'newline' return file stream
	)
)

;helper function (lexer)
;gets token as a string
;prints keywords to terminal according to gpplexer function
(defun print_lexer (token)
	(cond
		;if token is operator then print the related keyword
		((eq isOperator 1) 
			(cond
				((string-equal token OP_PLUS) (format t "OP_PLUS") )
				((string-equal token OP_MINUS) (format t "OP_MINUS") )
				((string-equal token OP_DIV) (format t "OP_DIV") )	
				((string-equal token OP_OP) (format t "OP_OP") )
				((string-equal token OP_CP) (format t "OP_CP") )
				((string-equal token OP_OC) (format t "OP_OC") )
				((string-equal token OP_CC) (format t "OP_CC") )	
			)

			(setf isOperator 0)
			(setf token_operator "")
		)

		;if token is dblmult operator then print the related keyword
		((and (eq isDblmult 1) (string-equal token OP_DBLMULT) )
			(format t "OP_DBLMULT") 
			(setf isDblmult 0)
			(setf token_mult "")	
		)

		;if token is mult operator then print the related keyword
		((and (eq isMult 1) (string-equal token OP_MULT) )
			(format t "OP_MULT") 
			(setf isMult 0)
			(setf token_mult "")	
		)

		;if token is reserved keyword then print the related keyword
		((eq isKEYorID 1)

			(setq syntax_control (subseq token 0 (- (length token) 1))) ;string to control syntax error for reserved words

			(cond
				
				;controls syntax error 
				((or (string-equal syntax_control KW_AND) (string-equal syntax_control KW_OR) (string-equal syntax_control KW_NOT) 
					 (string-equal syntax_control KW_EQUAL) (string-equal syntax_control KW_LESS) (string-equal syntax_control KW_NIL) 
					 (string-equal syntax_control KW_LIST) (string-equal syntax_control KW_APPEND) (string-equal syntax_control KW_CONCAT) 
					 (string-equal syntax_control KW_SET) (string-equal syntax_control KW_DEFFUN) (string-equal syntax_control KW_FOR) 
					 (string-equal syntax_control KW_IF) (string-equal syntax_control KW_EXIT) (string-equal syntax_control KW_LOAD) 	
					 (string-equal syntax_control KW_DISP) (string-equal syntax_control KW_TRUE) (string-equal syntax_control KW_FALSE) )

					(format t "SYNTAX ERROR ~S cannot be tokenized" token)
					(setf DFA 1) ;terminate program
				)

				((string-equal token KW_AND) (format t "KW_AND") )
				((string-equal token KW_OR) (format t "KW_OR") )
				((string-equal token KW_NOT) (format t "KW_NOT") )
				((string-equal token KW_EQUAL) (format t "KW_EQUAL") )
				((string-equal token KW_LESS) (format t "KW_LESS") )
				((string-equal token KW_NIL) (format t "KW_NIL")  )
				((string-equal token KW_LIST) (format t "KW_LIST") )
				((string-equal token KW_APPEND) (format t "KW_APPEND") )	
				((string-equal token KW_CONCAT) (format t "KW_CONCAT") )
				((string-equal token KW_SET) (format t "KW_SET") )
				((string-equal token KW_DEFFUN) (format t "KW_DEFFUN") )	
				((string-equal token KW_FOR) (format t "KW_FOR") )
				((string-equal token KW_IF) (format t "KW_IF") )
				((string-equal token KW_EXIT) (format t "KW_EXIT") )
				((string-equal token KW_LOAD) (format t "KW_LOAD") )	
				((string-equal token KW_DISP) (format t "KW_DISP") )
				((string-equal token KW_TRUE) (format t "KW_TRUE") )
				((string-equal token KW_FALSE) (format t "KW_FALSE") )
				(t 	(format t "IDENTIFIER") )
			)

			(setf isKEYorID 0)
			(setf isIdentifier 0)
			(setf token_key_id "")
		)

		;if token is comment then print the related keyword
		((eq isComment 1)
			(format t "COMMENT")
			(setf isComment 0)
			(setf token_comment "")
		)

		
		;if token is identifier then print the related keyword
		((eq isIdentifier 1)
			(format t "IDENTIFIER")
			(setf isIdentifier 0)
			(setf isKEYorID 0)
			(setf token_key_id "")
		)

		;if token integer the print the related keyword
		((eq isInteger 1) 

			(setf token (coerce token 'list)) ;converT token string into list

			(cond
				((and (> (length token) 1) (eq (car token) #\0 ) ) (format t "INVALID VALUE (zero leading)") ) ;integer value leadings 0
				(t (format t "VALUE") ) ;integer value
			)

			(setf isInteger 0)
			(setf token_int "")
		) 

	)
	(terpri) ;to print from new line
)

;TEST FUNCTION----------------------------------------------------------------------------------------------------------

(defun testLexicalAnalyzer ()
	(format t "Enter '$ g++' to open terminal.")
	(terpri)
	(format t "Enter '$ g++ filename.g++' to excute on file.")
	(terpri)
	(terpri)


	(gppinterpreter)
)


(testLexicalAnalyzer)
