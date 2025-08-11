# Performance Testing

## Mobibench

1. Clone the source repository of Mobibench fork for Turso:

```console
git clone git@github.com:penberg/Mobibench.git
```

2. Build Mobibench:

```console
cd Mobibench/shell
LIBS="../../target/release/libturso_sqlite3.a -lm" make
mv mobibench mobibench-turso
```

3. Run Mobibench:

(easiest way is to `cd` into `target/release`)

```console
# with strace, from target/release

strace -f -c ../../Mobibench/shell/mobibench-turso -f 1024 -r 4 -a 0 -y 0 -t 1 -d 0 -n 10000 -j 3 -s 2 -T 3 -D 1


./mobibench -p <benchmark-directory> -n 1000 -d 0 -j 4
```

## Clickbench

We have a modified version of the Clickbench benchmark script that can be run with:

```shell
make clickbench
```

This will build Turso in release mode, create a database, and run the benchmarks with a small subset of the Clickbench dataset.
It will run the queries for both Turso and SQLite, and print the results.



## Comparing VFS's/IO Back-ends (io_uring | syscall)

```shell
make bench-vfs SQL="select * from users;" N=500
```

The naive script will build and run limbo in release mode and execute the given SQL (against a copy of the `testing/testing.db` file)
`N` times with each `vfs`. This is not meant to be a definitive or thorough performance benchmark but serves to compare the two.


## TPC-H

1. Clone the Taratool TPC-H benchmarking tool:

```shell
git clone git@github.com:tarantool/tpch.git
```

2. Patch the benchmark runner script:

```patch
diff --git a/bench_queries.sh b/bench_queries.sh
index 6b894f9..c808e9a 100755
--- a/bench_queries.sh
+++ b/bench_queries.sh
@@ -4,7 +4,7 @@ function check_q {
        local query=queries/$*.sql
        (
                echo $query
-               time ( sqlite3 TPC-H.db < $query  > /dev/null )
+               time ( ../../limbo/target/release/limbo -m list TPC-H.db < $query  > /dev/null )
        )
 }
``` 

