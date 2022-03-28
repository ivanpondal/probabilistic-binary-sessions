while true; do
    inotifywait -e close_write -e delete -r .
    make
done
