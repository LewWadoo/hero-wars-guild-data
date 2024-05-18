(require 'emacsql-sqlite)

;; tested
(defvar guild-db-connection nil)

;; tested
(defun guild-db-connect ()
  (setq guild-db-connection (emacsql-sqlite "hero-wars-guild-data.db")))


(defun guild-db-create-tables ()
  (guild-db-create-table-ranks)
  (guild-db-create-table-players)
  (guild-db-create-table-roles)
  (guild-db-create-table-weeks)
  (guild-db-create-table-role-goals)
  (guild-db-create-table-player-week))
  
(defun guild-db-set-player-rank (player-id rank-id)
  "Set the rank of the player with the given PLAYER-ID to the rank with the given RANK-ID."
  (if (guild-db-get-player-rank player-id)
      (emacsql guild-db-connection
	       [:update player_ranks
			:set (= rank_id $s1)
			:where (= player_id $s2)]
	       rank-id player-id)
    (guild-db-create-player-rank player-id rank-id)))

(defun guild-db-create-player-rank (player-id rank-id)
  "Create a new player rank with the given PLAYER-ID and RANK-ID."
  (emacsql guild-db-connection
	   [:insert :into player_ranks ([player_id rank_id])
		    :values $v1] (vector player-id rank-id)))

(defun guild-db-create-table-player-ranks ()
  "Create the player_ranks table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists player_ranks
		    ([(player_id integer) (rank_id integer)]
		     (:unique [player-id])
		     (:foreign-key [player_id] :references players [id] :on-delete :cascade)
		     (:foreign-key [rank_id] :references ranks [id] :on-delete :cascade))]))

;; tested
(defun guild-db-create-table-ranks ()
  "Create the ranks table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists ranks
		    ([(id integer :primary-key) (rank_name text)])]))

;; tested
(defun guild-db-create-table-players ()
  "Create the players table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists players
		    ([(id integer :primary-key) (name text)])]))

;; tested
(defun guild-db-remove-table (table)
  "Remove the table if it exists."
  (emacsql guild-db-connection
	   [:drop-table :if :exists $i1] table))

;; tested
(defun guild-db-create-table-roles ()
  "Create the roles table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists roles
		    ([(id integer :primary-key) (name text :unique)])]))

;; tested
(defun guild-db-create-table-weeks ()
  "Create the weeks table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists weeks
		    ([(id integer :primary-key) (start_date date) (end_date date)]
		     (:unique [start_date end_date]))]))

;; tested
(defun guild-db-create-week (start-date end-date)
  "Create a week with START-DATE and END-DATE."
  (emacsql guild-db-connection
     [:insert-into weeks ([start_date end_date]) :values $v1] (vector start-date end-date)))

;; tested
(defun guild-db-create-table-role-goals ()
  "Create the role goals table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists role_goals
		    ([(id integer :primary-key) (role_id integer) (week_id integer) (goal integer)]
		     (:unique [role_id week_id])
		     (:foreign-key [role_id] :references roles [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade))]))

;; tested
(defun guild-db-create-table-player-week ()
  "Create the player week table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists player_week
		    ([(id integer :primary-key) (player_id integer) (week_id integer) (role1_id integer) (role2_id integer)]
		     (:unique [player-id week_id])
		     (:foreign-key [player_id] :references players [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade) (:foreign-key [role1_id] :references roles [id] :on-delete :cascade) (:foreign-key [role2_id] :references roles [id] :on-delete :cascade))]))


;; tested
(defun guild-db-create-player-week (player-id week-id role1-id role2-id)
  "Create a player week with PLAYER-ID, WEEK-ID, ROLE1-ID, and ROLE2-ID."
  (emacsql guild-db-connection
	   [:insert-into player_week ([player_id week_id role1_id role2_id]) :values $v1] (vector player-id week-id role1-id role2-id)))

(defun guild-db-remove-player-week-by-id (id)
  (emacsql guild-db-connection
	   [:delete :from player_week :where (= id $s1)] id))

;; tested
(defun guild-db-create-table-player-role-results ()
  "Create the player role results table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists player_role_results
		    ([(player_id integer) (week_id integer) (role_id integer) (result integer)]
		     (:unique [player_id week_id role_id])
		     (:foreign-key [player_id] :references players [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade) (:foreign-key [role_id] :references roles [id] :on-delete :cascade))]))

(defun guild-db-get-player-role-result (player-id week-id role-id)
  "Get the player role result from the database with the given PLAYER-ID, WEEK-ID, and ROLE-ID."
  (car (car (emacsql guild-db-connection
	   [:select [result] :from player_role_results
		    :where (and (= player_id $s1) (= week_id $s2) (= role_id $s3))]
	   player-id week-id role-id))))

(defun guild-db-create-player-role-result (player-id week-id role-id result)
  "Create the player role result in the database with the given PLAYER-ID, WEEK-ID, ROLE-ID, and RESULT."
  (emacsql guild-db-connection
	   [:insert-into player_role_results
			 ([player_id week_id role_id result])
			 :values $v1]
	   (vector player-id week-id role-id result)))

(defun guild-db-set-player-role-result (player-id week-id role-id result)
  "Set the player role result in the database with the given PLAYER-ID, WEEK-ID, ROLE-ID, and RESULT."
  (if (guild-db-get-player-role-result player-id week-id role-id)
      (emacsql guild-db-connection
	       [:update player_role_results
			:set ([(result)] $v1)
			:where (and (= player_id $s2) (= week_id $s3) (= role_id $s4))]
	       result player-id week-id role-id)
    (guild-db-create-player-role-result player-id week-id role-id result)))

  ;; (emacsql guild-db-connection
  ;; 	   [:update player_role_results
  ;; 		    :set ([(player_id week_id role_id result)] $v1)]
  ;; 	   (vector player-id week-id role-id result)))

;; tested
(defun guild-db-update-player-role-result (player-id week-id role-id result)
  "Update or create a player role result in the database with the given PLAYER-ID, WEEK-ID, ROLE-ID, and RESULT.
   If a result exists, add the RESULT to the existing value.
   Returns the resulting result value."
  (let* ((existing-results (emacsql guild-db-connection
                            [:select [result]
                             :from player_role_results
                             :where (and (= player_id $s1)
                                         (= week_id $s2)
                                         (= role_id $s3))] player-id week-id role-id))
         (result-exists (not (null existing-results)))
         (updated-result (if result-exists
                             (+ (car (car existing-results)) result)
                           result)))
    (if result-exists
        (emacsql guild-db-connection
          [:update player_role_results
           :set (= result $s4)
           :where (and (= player_id $s1)
                       (= week_id $s2)
                       (= role_id $s3))] player-id week-id role-id updated-result)
      (emacsql guild-db-connection
	       [:insert-into player_role_results ([player_id week_id role_id result])
			     :values $v1] (vector player-id week-id role-id updated-result)))
    updated-result))

(defun guild-db-read-player-role-result (player-id week-id role-id)
  "Read the player role result with PLAYER-ID, WEEK-ID, ROLE-ID."
  (car (car (emacsql guild-db-connection
		     [:select [result]
			      :from player_role_results
			      :where (and (= player_id $s1)
					  (= week_id $s2)
					  (= role_id $s3))] player-id week-id role-id))))

(defun guild-db-read-role-goal (week-id role-id)
  "Read the role goal with WEEK-ID, ROLE-ID."
  (car (car (emacsql guild-db-connection
		     [:select [goal]
			      :from role_goals
			      :where (and (= week_id $s1)
					  (= role_id $s2))] week-id role-id))))

;; (defun guild-db-update-player-role-result (player-id week-id role-id result)
;;   "Update the player role result with PLAYER-ID, WEEK-ID, ROLE-ID, and RESULT."
;;   (emacsql guild-db-connection
;; 	   [:update player_role_results
;; 		    :set (= result $v1)
;; 		    :where (and (= player_id $v2)
;; 				(= week_id $v3)
;; 				(= role_id $v4))]
;; 	   result player-id week-id role-id))


;; tested
(defun guild-db-create-rank (id rank-name)
  "Create a rank with the given name."
  (emacsql guild-db-connection
	   [:insert :into ranks ([id rank-name])
		    :values $v1] (vector id rank-name)))

;; tested
(defun guild-db-get-player-rank (player-id)
  "Get the rank id of the player with the given id."
  (car (car (emacsql guild-db-connection
		     [:select rank_id
			      :from player_ranks
			      :where (= player_id $s1)] player-id))))

(defun guild-db-get-rank-id-by-name (rank-name)
  "Get the rank id by the given name."
  (car (car (emacsql guild-db-connection
		     [:select [id]
			      :from ranks
			      :where (= rank-name $s1)] rank-name))))

;; tested
(defun guild-db-fill-ranks ()
  "Fill the ranks table with the default ranks."
  (guild-db-create-rank 0 "Not in guild")
  (guild-db-create-rank 1 "Member")
  (guild-db-create-rank 2 "Officer")
  (guild-db-create-rank 3 "General")
  (guild-db-create-rank 4 "Guild Master"))


(defun guild-db-fill-roles ()
  "Fill the roles table."
  (guild-db-create-role 1 "чемпион ВГ")
  (guild-db-create-role 2 "чемпион ГЧ")
  (guild-db-create-role 3 "активист")
  (guild-db-create-role 4 "шахтёр"))

;; tested
(defun guild-db-create-player (name player-id)
  "Create a player with the given name and id."
  (emacsql guild-db-connection
	   [:insert :into players ([id name])
		    :values $v1] (vector player-id name)))

(defun guild-db-rename-player (player-id new-name)
  "Rename the player with the given PLAYER-ID to the NEW-NAME."
  (emacsql guild-db-connection
	   [:update players
		    :set (= name $s1)
		    :where (= id $s2)]
	   new-name player-id))

;; tested
(defun guild-db-read-player (player-id)
  "Read a player with the given id."
  (let ((player-read (emacsql guild-db-connection
			       [:select * :from players :where (= id $s1)] player-id)))
    (car player-read)))

(defun guild-db-get-role-name-by-id (role-id)
  "Get role's name by its ROLE-ID."
  (let ((role-name (car (car (emacsql guild-db-connection
			      [:select role_name :from roles :where (= id $s1)] role-id)))))
    role-name))

(defun guild-db-get-player-name-by-id (player-id)
  "Get player's name by their PLAYER-ID."
  (let ((player-name (car (car (emacsql guild-db-connection
			      [:select name :from players :where (= id $s1)] player-id)))))
    player-name))
  
(defun guild-db-get-player-id-by-name (player-name)
  "Get player's ID by their PLAYER-NAME."
  (let ((player-ids (emacsql guild-db-connection
                      [:select id :from players :where (= name $s1)] player-name)))
    (if player-ids
        (car (car player-ids))
      (error "No player found with name: %s" player-name))))

;; tested
(defun guild-db-create-role (id role-name)
  "Create a role with the given id and name."
  (emacsql guild-db-connection
	   [:insert :into roles ([id role-name])
		    :values $v1] (vector id role-name)))

(defun guild-db-create-role-goal (week-id role-id goal)
  "Create a role goal with the given role id, week id and goal."
  (emacsql guild-db-connection
	   [:insert :into role_goals ([week_id role_id goal])
		    :values $v1] (vector week-id role-id goal)))

;; (defun guild-db-read-role-goals ()
;;   "Read the role goals for the given week id."
;;   (emacsql guild-db-connection
;; 	   [:select [role_goals:goal] :from role_goals]))

;; read role goals for the given week displaying the role names
;; (defun guild-db-read-role-goals-for-week (week-id)
;;   "Read the role goals for the given week id."
;;   (emacsql guild-db-connection
;; 	   [:select [roles:role_name role_goals:goal] :from role_goals
;; 		    :inner :join roles :on (= role_goals:role_id roles:id)
;; 		    :where (= role_goals:week_id $s1)] week-id))

(defun guild-db-read-role-goals-for-week (week-id)
  "Read the role goals for the given week id."
  (emacsql guild-db-connection
	   [:select goal :from role_goals
		    :where (= week_id $s1)] week-id))

;; (defun guild-db-read-role-goals (role-id)
;;   "Read the role goals for the given role id."
;;   (emacsql guild-db-connection
;; 	   [:select * :from role_goals :where (= role_id $s1)] role-id))

;; tested
(defun guild-db-read-roles ()
  "Read all roles."
  (emacsql guild-db-connection
	   [:select * :from roles]))

;; tested
(defun guild-db-read-weeks ()
  "Read all weeks."
  (emacsql guild-db-connection
	   [:select * :from weeks]))

;; tested
(defun guild-db-read-ranks ()
  "Read all ranks."
  (emacsql guild-db-connection
	   [:select * :from ranks]))

;; tested
(defun guild-db-read-player-week (player-id week-id)
  "Read the player week for the given player id and week id."
  (emacsql guild-db-connection
	   [:select * :from player_week :where (and (= player_id $s1)
						    (= week_id $s2))]
	   player-id week-id))

(defun guild-db-read-player-weeks (week-id)
  "Read the player weeks for the given WEEK-ID, sorted by name."
  (emacsql guild-db-connection
           [:select [pw:player_id pw:role1_id pw:role2_id]
                    :from (as player_week pw)
                    :inner-join (as players p)
                    :on (= pw:player_id p:id)
                    :where (= pw:week_id $s1)
                    :order-by (asc p:name)] week-id))

(defun guild-db-read-player-weeks-all (week-id)
  "Read the player weeks for the given week id."
  (let ((player-weeks (emacsql guild-db-connection
			       [:select * :from player_week :where (= week_id $s1)] week-id)))
    (dolist (player-week player-weeks)
      player-week)))
	
					; get the results of the given week for each player's roles with the comment whether a player reached the goal for each of their roles or failed with role names
(defun guild-db-read-player-week-results (week-id)
  "Read the player week results for the given week id."
  (emacsql guild-db-connection
	   [:select [p:name r:role_name pr:result rg:goal]
		    :from players :as p
		    :inner :join player_role_results :as pr
		    :on (= p:id pr:player_id)
		    :inner :join roles :as r
		    :on (= pr:role_id r:id)
		    :inner :join role_goals :as rg
		    :on (= pr:role_id rg:role_id)
		    :where (= pr:week_id $s1)] week-id))

;; (defun guild-db-get-results-for-week (week-id)
;;   "Retrieve the results of all players for the given WEEK-ID."
;;   (emacsql guild-db-connection
;;            [:select [p:name pr:role_id pr:result rg:goal]
;;                     :from players :as p
;;                     :join player_role_results :as pr
;;                     :on (= p:id pr:player_id)
;;                     :join role_goals :as rg
;;                     :on (and (= pr:role_id rg:role_id)
;;                              (= pr:week_id rg:week_id))
;;                     :where (= pr:week_id $s1)] week-id))

;; read players
(defun guild-db-read-players ()
  "Read all players."
  (emacsql guild-db-connection
	   [:select *
		    :from players]))

(defun guild-db-get-week-dates (week-id)
  "Get the dates of the week with the given WEEK-ID."
      (car (emacsql guild-db-connection
	       [:select [start_date end_date]
			:from weeks
			:where (= id $s1)] week-id)))

(defun guild-db-decrement-player-rank (player-id)
  "Decrement the rank of the player with the given PLAYER-ID."
  (let* ((rank-current (guild-db-get-player-rank player-id))
	 (rank-next (- rank-current 1)))
    (when (> rank-current 0)
      (emacsql guild-db-connection
	       [:update player_ranks
			:set (= rank_id $s1)
			:where (= player_id $s2)]
	       rank-next player-id))
    rank-next))

(defun guild-db-process-player-week (week-id)
  (let* ((player-week-roles (guild-db-read-player-weeks week-id))
         (role-goals (guild-db-read-role-goals-for-week week-id))
         (statement ""))
    (dolist (player-week-role player-week-roles)
      (let* ((player-id (car player-week-role))
             (role1-id (cadr player-week-role))
             (role2-id (caddr player-week-role))
             (player-name (guild-db-get-player-name-by-id player-id))
             (player-rank (guild-db-get-player-rank player-id))
             (did-not-complete-goals '()))
        (dolist (role-id (list role1-id role2-id))
          (let* ((role-name (guild-db-get-role-name-by-id role-id))
                 (player-role-result (guild-db-read-player-role-result player-id week-id role-id))
                 (goal (car (nth (1- role-id) role-goals))))
            (unless (and player-role-result (>= player-role-result goal))
              (push role-name did-not-complete-goals))))
        (when did-not-complete-goals
          (setq statement (concat statement
                                 (format "%s не выполнил нормы %s: %s — "
                                         player-name
                                         (if (eq (length did-not-complete-goals) 1) "роли" "ролей")
                                         (string-join did-not-complete-goals ", "))))
          (if (<= player-rank 1)
	      (setq statement (concat statement "изгоняется"))
	    (setq statement (concat statement "разжалуется")))
	  (setq statement (concat statement "\n"))
          (guild-db-decrement-player-rank player-id))))
    statement))



;; (defun guild-db-process-player-week (week-id)
;;   (let* ((player-week-roles (guild-db-read-player-weeks week-id))
;;          (role-goals (guild-db-read-role-goals-for-week week-id))
;;          (statement ""))
;;     (dolist (player-week-role player-week-roles)
;;       (let* ((player-id (car player-week-role))
;;              (role1-id (cadr player-week-role))
;;              (role2-id (caddr player-week-role))
;;              (player-name (guild-db-get-player-name-by-id player-id))
;;              (did-not-complete-goals '()))
;;         (dolist (role-id (list role1-id role2-id))
;;           (let* ((role-name (guild-db-get-role-name-by-id role-id))
;;                  (player-role-result (guild-db-read-player-role-result player-id week-id role-id))
;;                  (goal (car (nth (1- role-id) role-goals))))
;;             (unless (and player-role-result (>= player-role-result goal))
;;               (push role-name did-not-complete-goals))))
;;         (when did-not-complete-goals
;;           (setq
;;            statement (concat statement
;;                              (format "%s не выполнил нормы %s: %s\n"
;;                                      player-name
;;                                      (if (eq (length did-not-complete-goals) 1) "роли" "ролей")
;;                                      (string-join did-not-complete-goals ", ")))))))
;;     statement))


;; (defun guild-db-process-player-week (week-id)
;;   (let* ((player-week-roles (guild-db-read-player-weeks week-id))
;;          (role-goals (guild-db-read-role-goals-for-week week-id))
;;          (statement ""))
;;     (dolist (player-week-role player-week-roles)
;;       (let* ((player-id (car player-week-role))
;;              (role1-id (cadr player-week-role))
;;              (role2-id (caddr player-week-role))
;;              (player-name (guild-db-get-player-name-by-id player-id))
;;              (did-not-complete-goals '()))
;;         (dolist (role-id (list role1-id role2-id))
;;           (let* ((role-name (guild-db-get-role-name-by-id role-id))
;;                  (player-role-result (guild-db-read-player-role-result player-id week-id role-id))
;;                  (goal (car (nth (1- role-id) role-goals))))
;;             (unless (and player-role-result (>= player-role-result goal))
;;               (push role-name did-not-complete-goals))))
;;         (when did-not-complete-goals
;;           (setq statement (concat statement
;;                                  (format "%s не выполнил нормы роли: %s\n"
;;                                          player-name
;;                                          (string-join did-not-complete-goals ", ")))))))
;;     statement))


;; (defun guild-db-process-player-week (week-id)
;;   (let ((player-week-roles (guild-db-read-player-weeks week-id))
;; 	(role-goals (guild-db-read-role-goals-for-week week-id))
;; 	(statement ""))
;;     (dolist (player-week-role player-week-roles)
;;       (let ((player-id (car player-week-role))
;; 	    (role1-id (cadr player-week-role))
;; 	    (role2-id (caddr player-week-role)))
;; 	(let ((player-name (guild-db-get-player-name-by-id player-id)))
;; 	  (dolist (role-id (list role1-id role2-id))
;; 	    (let* ((role-name (guild-db-get-role-name-by-id role-id))
;; 		   (player-role-result (guild-db-read-player-role-result player-id week-id role-id))
;; 		   (reached-p (and player-role-result (>= player-role-result (car (nth (1- role-id) role-goals))))))
;; 	      (or reached-p (setq statement (concat statement (format "%s не выполнил нормы роли «%s»\n" player-name role-name)))))))))
;;     statement))

(defun guild-db-get-player-roles-to-publish (week-id)
  (let* ((player-week-roles (guild-db-read-player-weeks week-id))
	(week-dates (guild-db-get-week-dates week-id))
	(message-for-guild-announcement (format "роли на %s—%s:\n" (car week-dates) (cadr week-dates)))
	(count-player 1))
    (dolist (player-week-role player-week-roles)
      (let ((player-id (car player-week-role))
	    (role1-id (cadr player-week-role))
	    (role2-id (caddr player-week-role)))
	(let ((player-name (guild-db-get-player-name-by-id player-id)))
	  (setq message-for-guild-announcement (concat message-for-guild-announcement (format "%d) %s: " count-player player-name)))
	  (setq count-player (1+ count-player))
	  (dolist (role-id (list role1-id role2-id))
	    (let* ((role-name (guild-db-get-role-name-by-id role-id))
		  (divider (if (eq role1-id role-id) ", " "\n")))
	      (setq message-for-guild-announcement (concat message-for-guild-announcement (format "%s%s" role-name divider))))))))
    message-for-guild-announcement))

(defun guild-db-generate-result-function-calls (week-id)
  (let* ((player-week-roles (guild-db-read-player-weeks week-id))
         (result-function-calls ""))
    (dolist (player-week-role player-week-roles)
      (let* ((player-id (car player-week-role))
             (role1-id (cadr player-week-role))
             (role2-id (caddr player-week-role))
             (result-function-call1 (format "; %s\n(guild-db-update-player-role-result %s %s %s "
                                            (guild-db-get-player-name-by-id player-id) player-id week-id role1-id))
             (result-function-call2 (format "(guild-db-update-player-role-result %s %s %s "
                                            player-id week-id role2-id)))
        (setq result-function-calls (concat result-function-calls result-function-call1 "\n" result-function-call2 "\n"))))
    result-function-calls))

(defun guild-db-generate-create-player-week (player-name week-id)
  (let ((player-id (guild-db-get-player-id-by-name player-name)))
    (format "; %s\n(guild-db-create-player-week %s %s "
            player-name player-id week-id)))

(defun guild-db-get-all-player-names-list ()
  "Retrieve a list of all players from the database."
  (let ((result (emacsql guild-db-connection
                  [:select [name] :from players
                   :order-by (asc name)])))
    (mapcar #'car result)))

(defun guild-db-get-all-rank-names-list ()
  "Retrieve a list of all player ranks from the database."
  (let ((result (emacsql guild-db-connection
			 [:select [rank-name] :from ranks
				  :order-by (asc rank-name)])))
    (mapcar #'car result)))

(defun guild-db-get-last-week-id ()
  "Retrieve the ID of the last week."
  (let ((result (emacsql guild-db-connection
			 [:select [id] :from weeks
				  :order-by (desc id)
				  :limit 1])))
    (car (car result))))

(provide 'guild-db)
