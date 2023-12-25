(require 'emacsql-sqlite)

(defvar guild-db-connection nil)

;; tested
(defun guild-db-connect ()
  (setq guild-db-connection (emacsql-sqlite "hero-wars-guild-data.db"))
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
		    ([(id integer :primary-key) (name text) (rank_id integer)] (:foreign-key [rank_id] :references ranks [id] :on-delete :cascade))]))


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
(defun guild-db-create-table-role-goals ()
  "Create the role goals table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists role_goals
		    ([(id integer :primary-key) (role_id text) (week_id integer) (goal integer)] (:foreign-key [role_id] :references roles [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade))]))

;; tested
(defun guild-db-create-table-player-roles ()
  "Create the player roles table if it doesn't exist."
  (emacsql guild-db-connection
	   [:create :table :if :not :exists player_roles
		    ([(id integer :primary-key) (player_id integer) (role_id text) (week_id integer) (actual integer)] (:foreign-key [player_id] :references players [id] :on-delete :cascade) (:foreign-key [role_id] :references roles [id] :on-delete :cascade) (:foreign-key [week_id] :references weeks [id] :on-delete :cascade))]))

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


;; tested
(defun guild-db-create-player (name player-id rank-id)
  "Create a player with the given name and id."
  (emacsql guild-db-connection
	   [:insert :into players ([id name rank_id])
		    :values $v1] (vector player-id name rank-id)))


;; tested
(defun guild-db-read-player (player-id)
  "Read a player with the given id."
  (let* ((player-read (emacsql guild-db-connection
			       [:select * :from players :where (= id $s1)] player-id)))
    (car player-read)))

(provide 'guild-db)
