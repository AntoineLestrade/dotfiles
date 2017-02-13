pushd %~dp0
if not exist emacs\libs\ mkdir "emacs\libs"
if not exist emacs\libs\use-package (
   mkdir "emacs\libs\use-package"
)

curl https://raw.githubusercontent.com/jwiegley/use-package/master/use-package.el -k -o "emacs\libs\use-package\use-package.el"
curl https://raw.githubusercontent.com/jwiegley/use-package/master/bind-key.el -k -o "emacs\libs\use-package\bind-key.el"

popd
