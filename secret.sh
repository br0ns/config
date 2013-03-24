rm -f secret.tar secret.tar.gpg
tar cfv secret.tar secret
gpg -c secret.tar && rm secret.tar
git add secret.tar.gpg
git commit -m "secret"
git push