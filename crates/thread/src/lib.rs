#![warn(rust_2018_idioms, unreachable_pub)]

use std::{mem, ptr};

pub struct Pool {
	// `job_tx` must be above `_handles` for drop order.
	// `_handles` isn’t used anywhere, it’s present only for Drop.
	job_tx: flume::Sender<Job>,
	_handles: Vec<JoinHandle<()>>,
}

type Job = Box<dyn FnOnce() + Send>;

impl Pool {
	pub fn new() -> Pool {
		let core_count = core_count();
		let mut handles = Vec::with_capacity(core_count as usize);

		let (job_tx, job_rx) = flume::bounded::<Job>(core_count as usize * 2);

		for _ in 0..core_count {
			let job_rx = job_rx.clone();
			handles.push(spawn(QosClass::Utility, || {
				for job in job_rx {
					job();
				}
			}));
		}

		Pool { _handles: handles, job_tx }
	}

	pub fn submit_job<F>(&self, job: F)
	where
		F: FnOnce() + Send + 'static,
	{
		let job: Job = Box::new(job);
		self.job_tx.send(job).unwrap();
	}
}

impl Default for Pool {
	fn default() -> Self {
		Self::new()
	}
}

fn core_count() -> u32 {
	let mut core_count = 0;
	let mut byte_count = mem::size_of::<u32>();

	let ret = unsafe {
		libc::sysctlbyname(
			"hw.logicalcpu".as_ptr().cast(),
			(&mut core_count as *mut u32).cast(),
			&mut byte_count,
			ptr::null_mut(),
			0,
		)
	};

	// None of the possible failure reasons for `sysctlbyname`
	// should ever occur.
	assert_eq!(ret, 0);

	assert_eq!(byte_count, mem::size_of::<u32>());
	assert_ne!(core_count, 0);

	core_count
}

fn spawn<F, T>(qos_class: QosClass, f: F) -> JoinHandle<T>
where
	F: FnOnce() -> T + Send + 'static,
	T: Send + 'static,
{
	let handle = std::thread::spawn(|| {
		set_qos_class(qos_class);
		f()
	});
	JoinHandle { inner: Some(handle) }
}

struct JoinHandle<T = ()> {
	inner: Option<std::thread::JoinHandle<T>>,
}

impl<T> Drop for JoinHandle<T> {
	fn drop(&mut self) {
		let handle = self.inner.take().unwrap();
		handle.join().unwrap();
	}
}

pub enum QosClass {
	UserInteractive,
	UserInitiated,
	Utility,
	Background,
}

pub fn set_qos_class(qos_class: QosClass) {
	let qos_class = match qos_class {
		QosClass::UserInteractive => libc::qos_class_t::QOS_CLASS_USER_INTERACTIVE,
		QosClass::UserInitiated => libc::qos_class_t::QOS_CLASS_USER_INITIATED,
		QosClass::Utility => libc::qos_class_t::QOS_CLASS_UTILITY,
		QosClass::Background => libc::qos_class_t::QOS_CLASS_BACKGROUND,
	};

	let ret = unsafe { libc::pthread_set_qos_class_self_np(qos_class, 0) };
	assert_eq!(ret, 0);
}
