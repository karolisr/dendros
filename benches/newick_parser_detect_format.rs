use criterion::{
    BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
};
use dendros::parse_trees;
use std::fs;
use std::hint::black_box;
use std::path::PathBuf;

fn prepare_test_data() -> Vec<(String, String)> {
    let mut test_data = Vec::new();
    let test_data_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data");

    let newick_files = [
        "carnivore.tre", "influenza.tre",
        "influenza.no.single.quotes.on.tips.tre", "tree01.tre",
    ];

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

fn bench_newick_parser_detect_format(c: &mut Criterion) {
    let test_data = prepare_test_data();

    let mut group = c.benchmark_group("newick_parser_detect_format");

    for (name, newick_string) in &test_data {
        let char_count = newick_string.len();
        let _ = group.throughput(Throughput::Bytes(char_count as u64));

        let _ = group.bench_with_input(
            BenchmarkId::new("parse_newick_detect_format", name),
            newick_string,
            |b, newick| {
                b.iter(|| {
                    let _ = black_box(parse_trees(newick.clone()));
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
        criterion = criterion.measurement_time(std::time::Duration::from_secs(15));
        criterion = criterion.sample_size(15);
        criterion
    };
    targets = bench_newick_parser_detect_format
);
criterion_main!(benches);
