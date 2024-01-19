read dir?"Enter root directory in which to start search: "
read name?"Enter file name string: "
find "$dir" -name "$name" -print | more
