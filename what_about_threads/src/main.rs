use env_logger; // 0.9.3
use log::{info, LevelFilter, error}; // 0.4.17
use rayon;
use std::collections::{HashMap, HashSet};
// 1.6.0
use std::io::Write;
use crossbeam_channel;

// Unique identifier of work. If there are no fields work is unique.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum WorkIdentifier {
    GlobalMetadata,
    GlyphIr(u32),
    FinishIr,
}

trait Work {
    fn exec(&self);
}

struct StringWork {
    message: String,
}

impl Work for StringWork {
    fn exec(&self) {
        info!("{}", self.message);
    }
}

impl StringWork {
    fn new(message: String) -> impl Work {
        StringWork { message }
    }
}

fn create_work() -> (HashMap<WorkIdentifier, Box<dyn Work + Send>>, HashMap<WorkIdentifier, HashSet<WorkIdentifier>>) {    
    let mut work: HashMap<WorkIdentifier, Box<dyn Work + Send>> = HashMap::new();
    // entry i requires entries at <indices> to complete before running
    let mut deps: HashMap<WorkIdentifier, HashSet<WorkIdentifier>> = HashMap::new();    

    // Global metadata depends on nothing and nobody!
    work.insert(WorkIdentifier::GlobalMetadata, Box::new(StringWork::new("Global Metadata".to_string())));
    deps.insert(WorkIdentifier::GlobalMetadata, Default::default());

    for i in 0..10 {
        let work_id = WorkIdentifier::GlyphIr(i);
        work.insert(work_id, Box::new(StringWork::new(format!("Hi {}", i))));

        // Glyphs require global metadata is done
        deps.entry(work_id).or_default().insert(WorkIdentifier::GlobalMetadata);
    }

    // Finish IR requires all tasks except finish IR be done so take all the keys prior to adding it
    deps.insert(WorkIdentifier::FinishIr, work.keys().into_iter().copied().collect());
    work.insert(WorkIdentifier::FinishIr, Box::new(StringWork::new("Finish Ir".to_string())));

    // It would be Very Bad if we modified this code such that this was not true
    assert_eq!(work.keys().into_iter().collect::<HashSet<&WorkIdentifier>>(), deps.keys().into_iter().collect::<HashSet<&WorkIdentifier>>());
    (work, deps)
}

fn main() {
    env_logger::builder()
        .filter(None, LevelFilter::Info)
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            writeln!(
                buf,
                "{}: {:?}: {}: {}",
                ts,
                std::thread::current().id(),
                buf.default_level_style(record.level())
                    .value(record.level()),
                record.args()
            )
        })
        .init();

    info!("main");
    
    let (send, recv) = crossbeam_channel::unbounded::<WorkIdentifier>();
    rayon::in_place_scope(|scope| {
        info!("in_place");
        let (mut work, mut deps) = create_work();
        let work_count = work.len();
        let mut reverse_deps: HashMap<WorkIdentifier, Vec<WorkIdentifier>> = HashMap::new();
        for (work, deps) in deps.iter() {
            for dep in deps {
                reverse_deps.entry(*dep).or_default().push(*work);
            }
        }

        // Whenever a task completes see if it was the last incomplete dependency of other task(s)
        // and spawn them if it was
        // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress
        let mut complete = HashSet::new();
        while complete.len() < work_count {
            // Spawn anything that is currently executable
            for (id, deps) in deps.iter() {
                if !work.contains_key(&id) || !deps.is_empty() {    
                    if !deps.is_empty() {                
                        info!("Cannot start {:?}, blocked on {:?}", id, deps);
                    }
                    continue;
                }
                info!("Start {:?}", id);
                let id = id.clone();
                let work = work.remove(&id).unwrap();
                let send = send.clone();
                scope.spawn(move |_| {
                    work.exec();
                    if let Err(e) = send.send(id.clone()) {
                        error!("Unable to write {:?} to completion channel: {}", id, e);
                    }
                })
            }

            // Block for something to phone home to say it's done
            // Then complete everything that has reported since our last check
            let mut opt_complete = Some(recv.recv().unwrap());  // blocks
            while let Some(completed_id) = opt_complete.take() {
                if !complete.insert(completed_id.clone()) {
                    panic!("Repeat signals for completion of {:#?}", completed_id);
                }
                deps.remove(&completed_id);
                info!("{}/{} complete, most recently {:?}", complete.len(), work_count, completed_id);
                if let Some(ids) = reverse_deps.get(&completed_id) {
                    ids.into_iter().for_each(|dependent_id| {
                        let deps = deps.get_mut(dependent_id).unwrap();
                        deps.remove(&completed_id);
                        if deps.is_empty() {
                            info!("{:?} unblocked", dependent_id);
                        }
                    });
                }

                // Update completed id to the next complete item, if any
                if let Ok(completed_id) =  recv.try_recv() {
                    opt_complete = Some(completed_id);
                }
            }
        }

    });
}
