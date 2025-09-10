use bullet_lib::{
    game::inputs::ChessBucketsMirrored,
    nn::optimiser::AdamW,
    trainer::{
        save::SavedFormat,
        schedule::{TrainingSchedule, TrainingSteps, lr, wdl},
        settings::LocalSettings,
    },
    value::{ValueTrainerBuilder, loader::DirectSequentialDataLoader},
};

fn main() {
    // hyperparams to fiddle with
    let hl_size = 1024;
    let dataset_path = "data/baseline.bin";
    let warmup_path = "data/warmup.bin";
    let initial_lr = 0.001;
    let final_lr = initial_lr * 0.3f32.powi(5);

    let start_wdl = 0.2;
    let end_wdl = 0.5;

    let warmup_superbatches = 10; // 3 epochs
    let superbatches = 240; // 20 epochs
    let qa = 255;
    let qb = 64;

    let scale = 400;

    let buckets = ChessBucketsMirrored::new([0; 32]);

    let mut trainer = ValueTrainerBuilder::default()
        .dual_perspective()
        .optimiser(AdamW)
        .inputs(buckets)
        .save_format(&[
            SavedFormat::id("l0w").quantise::<i16>(qa),
            SavedFormat::id("l0b").quantise::<i16>(qa),
            SavedFormat::id("l1w").quantise::<i16>(qb),
            SavedFormat::id("l1b").quantise::<i16>(qa * qb),
        ])
        .loss_fn(|output, target| output.sigmoid().squared_error(target))
        .build(|builder, stm_inputs, ntm_inputs| {
            // weights
            let l0 = builder.new_affine("l0", 768, hl_size);
            let l1 = builder.new_affine("l1", 2 * hl_size, 1);

            // inference
            let stm_hidden = l0.forward(stm_inputs).screlu();
            let ntm_hidden = l0.forward(ntm_inputs).screlu();
            let hidden_layer = stm_hidden.concat(ntm_hidden);
            l1.forward(hidden_layer)
        });

    let settings = LocalSettings {
        threads: 4,
        test_set: None,
        output_directory: "checkpoints",
        batch_queue_size: 64,
    };

    let base_name = format!("screlu{hl_size}_hm");
    let warmup_schedule = TrainingSchedule {
        net_id: format!("{base_name}_warmup"),
        eval_scale: scale as f32,
        steps: TrainingSteps {
            batch_size: 16_384,
            batches_per_superbatch: 6104,
            start_superbatch: 1,
            end_superbatch: warmup_superbatches,
        },
        wdl_scheduler: wdl::LinearWDL {
            start: 0.0,
            end: start_wdl,
        },
        lr_scheduler: lr::ConstantLR { value: initial_lr },
        save_rate: 10,
    };

    let dataloader = DirectSequentialDataLoader::new(&[warmup_path]);

    trainer.run(&warmup_schedule, &settings, &dataloader);

    let main_schedule = TrainingSchedule {
        net_id: base_name,
        eval_scale: scale as f32,
        steps: TrainingSteps {
            batch_size: 16_384,
            batches_per_superbatch: 6104,
            start_superbatch: 1,
            end_superbatch: superbatches,
        },
        wdl_scheduler: wdl::LinearWDL {
            start: start_wdl,
            end: end_wdl,
        },
        lr_scheduler: lr::CosineDecayLR {
            initial_lr,
            final_lr,
            final_superbatch: superbatches,
        },
        save_rate: 10,
    };

    let dataloader = DirectSequentialDataLoader::new(&[dataset_path]);

    trainer.run(&main_schedule, &settings, &dataloader);
}
