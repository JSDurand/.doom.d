;; -*- no-byte-compile: t; -*-
;;; completion/durand-completion/packages.el

(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "c6432b086f9c8ccb50b3656ba5895750f2a15541")

(package! icomplete-vertical
  :recipe (:host github :repo "oantolin/icomplete-vertical")
  :pin "ec7258c5adf9a154d2ae6d37719811d43b5611ab")

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "b6974f98832fe1433d8f0efe4bd362b665f57b98")
