use criterion::{
    BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
};
use dendros::parse_newick;
use std::fs;
use std::hint::black_box;
use std::path::PathBuf;

fn prepare_test_data() -> Vec<(String, String)> {
    let mut test_data = Vec::new();
    let test_data_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data");

    let newick_files = ["tree01.tre"];

    for file_path in &newick_files {
        let full_path = test_data_dir.join(file_path);
        if let Ok(content) = fs::read_to_string(&full_path) {
            let name = file_path
                .replace('/', "_")
                .replace(".newick", "")
                .replace(".tre", "");
            test_data.push((name, content));
        }
    }

    test_data
}

fn bench_newick_parser(c: &mut Criterion) {
    let test_data = prepare_test_data();

    let mut group = c.benchmark_group("newick_parser");
    let _ = group.sample_size(30);

    for (name, newick_string) in &test_data {
        let char_count = newick_string.len();
        let _ = group.throughput(Throughput::Bytes(char_count as u64));

        let _ = group.bench_with_input(
            BenchmarkId::new("parse_newick", name),
            newick_string,
            |b, newick| {
                b.iter(|| {
                    let _ = black_box(parse_newick(newick.clone()));
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    name = benches;
    config = {
        let mut criterion = Criterion::default();
        let benchmark_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("benchmark_results");
        criterion = criterion.output_directory(&benchmark_dir);
        criterion = criterion.warm_up_time(std::time::Duration::from_millis(500));
        criterion = criterion.measurement_time(std::time::Duration::from_secs(5));
        criterion
    };
    targets = bench_newick_parser
);
criterion_main!(benches);
