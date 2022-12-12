;; TODO: Make this file properly literate!
(defclass functor () ())
(defmethod <$> (f (a functor))
  (error "<$> must be defined by a subclass"))
(defmethod <$ (a b)
  (<$> (constantly a) b))

(defclass my-list (functor) ((x :initform '(1 2 3))))
(defmethod <$> (f (a my-list))
  (mapcar f (slot-value a 'x)))
(defmethod <$> (f (a list))
  (mapcar f a))
