# Copy the specified file to the current directory, renaming each by prefixing
# it with the PDF page label of its first page. Requires pdftk.

set -eu

in="$1"
in_file="$(basename "$1")"

prefix=$(gtimeout --signal=QUIT 10s pdftk "$in" dump_data output - | grep 'PageLabelPrefix' | awk '{print $2}')
start=$(gtimeout --signal=QUIT 10s pdftk "$in" dump_data output - | grep 'PageLabelStart' | awk '{print $2}')

num="$prefix$start"

if [[ $num ]]; then
    echo "Renaming $in_file to $num $in_file"
    cp "$in" "$num $in_file"
else
    echo "No page number for $in_file"
fi
