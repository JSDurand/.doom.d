;;; ui/transpose-frame/config.el -*- lexical-binding: t; -*-

(map! :leader
      :n [?w ?T] 'transpose-frame
      :n [?w ?y] 'rotate-frame-clockwise
      :n [?w ?e] 'rotate-frame-anticlockwise
      :n [?w ?g] 'rotate-frame)
