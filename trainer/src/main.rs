/*
This is about as simple as you can get with a network, the arch is
    (768 -> HIDDEN_SIZE)x2 -> 1
and the training schedule is pretty sensible.
There's potentially a lot of elo available by adjusting the wdl
and lr schedulers, depending on your dataset.
*/
use bullet_lib::{
    nn::{Activation, optimiser},
    trainer::{
        default::{Loss, TrainerBuilder, inputs, loader},
        schedule::{TrainingSchedule, TrainingSteps, lr, wdl},
        settings::LocalSettings,
    },
};

const HIDDEN_SIZE: usize = 512;
const SCALE: i32 = 400;
const QA: i16 = 255;
const QB: i16 = 64;

fn main() {
    let mut trainer = TrainerBuilder::default()
        .quantisations(&[QA, QB])
        .optimiser(optimiser::AdamW)
        .loss_fn(Loss::SigmoidMSE)
        .input(inputs::Chess768)
        .feature_transformer(HIDDEN_SIZE)
        .activate(Activation::SCReLU)
        .add_layer(1)
        .build();

    let schedule = TrainingSchedule {
        net_id: "screlu512".to_string(),
        eval_scale: SCALE as f32,
        steps: TrainingSteps {
            batch_size: 16_384,
            batches_per_superbatch: 6104,
            start_superbatch: 1,
            end_superbatch: 40,
        },
        wdl_scheduler: wdl::ConstantWDL { value: 0.5 },
        lr_scheduler: lr::StepLR {
            start: 0.001,
            gamma: 0.1,
            step: 18,
        },
        save_rate: 10,
    };

    trainer.set_optimiser_params(optimiser::AdamWParams::default());

    let settings = LocalSettings {
        threads: 4,
        test_set: None,
        output_directory: "checkpoints",
        batch_queue_size: 64,
    };

    // loading directly from a `BulletFormat` file
    let data_loader = loader::DirectSequentialDataLoader::new(&["../lunar_bak/data/baseline.bin"]);

    trainer.run(&schedule, &settings, &data_loader);
}
