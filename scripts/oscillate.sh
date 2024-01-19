# create sorted list of published table numbers to process as input to the oscillate.f90 program
grep -i "published" m:/graf2000/subject_filter.csv | awk 'BEGIN{FS=","};{print $2}' | sort -n > oscillate_tables.txt
