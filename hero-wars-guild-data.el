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
  (guild-db-create-table-player-roles))
  
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
(defun guild-db-remove-table-role-goals ()
  "Remove the role-goals table if it exists."
  (emacsql guild-db-connection
	   [:drop-table :if :exists role_goals]))

;; tested
(defun guild-db-remove-table-players ()
  "Remove the players table if it exists."
  (emacsql guild-db-connection
	   [:drop-table :if :exists players]))



;; tested
(defun guild-db-create-table-roles ()
  "Create the roles table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists roles
		    ([(id integer :primary-key) (role_name text)])]))

;; tested
(defun guild-db-create-table-weeks ()
  "Create the weeks table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists weeks
		    ([(id integer :primary-key) (start_date date) (end_date date)])]))

;; tested
(defun guild-db-create-week (id start-date end-date)
  "Create a week with START-DATE and END-DATE."
  (emacsql guild-db-connection
     [:insert-into weeks :values ([$s1 $s2 $s3])]
     id start-date end-date))

;; tested
(defun guild-db-create-table-role-goals ()
  "Create the role goals table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists role_goals
		    ([(id integer :primary-key) (role_id text) (week_id integer) (goal integer)] (:foreign-key [role_id] :references roles [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade))]))

;; tested
(defun guild-db-create-table-player-week ()
  "Create the player week table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists player_week
		    ([(id integer :primary-key) (player_id integer) (week_id integer) (rank_id integer) (role1_id integer) (role2_id integer)] (:foreign-key [player_id] :references players [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade) (:foreign-key [rank_id] :references ranks [id] :on-delete :cascade) (:foreign-key [role1_id] :references roles [id] :on-delete :cascade) (:foreign-key [role2_id] :references roles [id] :on-delete :cascade))]))


(defun guild-db-create-player-week (player-id week-id rank-id role1-id role2-id)
  "Create a player week with PLAYER-ID, WEEK-ID, RANK-ID, ROLE1-ID, and ROLE2-ID."
  (emacsql guild-db-connection
	   [:insert-into player_week ([player_id week_id rank_id role1_id role2_id]) :values $v1] (vector player-id week-id rank-id role1-id role2-id)))

;; tested
(defun guild-db-remove-table-player-roles ()
  "Remove the player roles table if it exists."
  (emacsql guild-db-connection
	   [:drop :table :if :exists player_roles]))

;; tested
(defun guild-db-create-rank (id rank-name)
  "Create a rank with the given name."
  (emacsql guild-db-connection
	   [:insert :into ranks ([id rank-name])
		    :values $v1] (vector id rank-name)))

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


;; tested
(defun guild-db-read-player (player-id)
  "Read a player with the given id."
  (let* ((player-read (emacsql guild-db-connection
			       [:select * :from players :where (= id $s1)] player-id)))
    (car player-read)))

;; tested
(defun guild-db-create-role (id role-name)
  "Create a role with the given id and name."
  (emacsql guild-db-connection
	   [:insert :into roles ([id role-name])
		    :values $v1] (vector id role-name)))

(defun guild-db-create-role-goal (role-id week-id goal)
  "Create a role goal with the given role id, week id and goal."
  (emacsql guild-db-connection
	   [:insert :into role_goals ([role_id week_id goal])
		    :values $v1] (vector role-id week-id goal)))

(defun guild-db-read-role-goals (role-id)
  "Read the role goals for the given role id."
  (emacsql guild-db-connection
	   [:select * :from role_goals :where (= role_id $s1)] role-id))

;; tested
(defun guild-db-read-roles ()
  "Read all roles."
  (emacsql guild-db-connection
	   [:select * :from roles]))

;; tested
(defun guild-db-read-ranks ()
  "Read all ranks."
  (emacsql guild-db-connection
	   [:select * :from ranks]))


(provide 'guild-db)
