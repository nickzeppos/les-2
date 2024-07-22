find . -type f -print0 | while IFS= read -r -d '' file; do
    dir=$(dirname "$file")
    base=$(basename "$file")
    IFS='/' read -ra ADDR <<< "$dir"
    newname="${dir}/BILLS-${ADDR[1]}${ADDR[3]}${ADDR[5]}.html"
    mv "$file" "$newname"
done
