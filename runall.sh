FILES=("day01" "day02" "day03" "day04" "day05" "day06" "day07" "day08" "day09")
FILES+=("day10" "day11" "day12" "day13" "day14")
FILES+=("day21")
# setup
rm -rf _build
mkdir _build
cd _build

cp -r ../inputs .

for file in ${FILES[@]}; do
  cp ../"$file".ml .
  ocamlopt -c "$file".ml
done

# make binary
cmxs=()
for file in ${FILES[@]}; do
  cmxs+=("$file".cmx)
done
ocamlopt -o all "${cmxs[@]}"

# run
./all

# cleanup
cd ..
rm -rf _build