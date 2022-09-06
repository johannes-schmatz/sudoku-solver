// syntax
// vim: ts=8 sts=8 sw=8 noet si syntax=rust

#[derive(Copy, Clone, Debug)]
struct SudokuNumberSet {
	numbers: [bool; 9],
}

impl SudokuNumberSet {
	fn new() -> Self {
		SudokuNumberSet {
			numbers: [false; 9],
		}
	}

	fn new_all() -> Self {
		SudokuNumberSet {
			numbers: [true; 9],
		}
	}

	fn invert(&self) -> Self {
		let mut copy = Self::new();
		for i in 0..9 {
			copy.numbers[i] = !self.numbers[i];
		}
		copy
	}

	fn count_true(&self) -> i32 {
		let mut count = 0;
		for i in 0..9 {
			if self.numbers[i] {
				count += 1;
			}
		}
		count
	}

	fn get_one(&self) -> Option<usize> {
		if self.count_true() == 1 {
			self.numbers.iter().position(|&x: &bool| -> bool {
				x
			})
		} else {
			None
		}
	}

	fn contains_all(&self) -> bool {
		self.count_true() == 9
	}

}

struct SudokuNumberSetIterator {
	set: SudokuNumberSet,
	pos: usize,
}

impl Iterator for SudokuNumberSetIterator {
	type Item = Option<usize>;

	fn next(&mut self) -> Option<<Self as Iterator>::Item> {
		if self.pos >= 9 {
			return None;
		}
		self.pos += 1;
		if self.set.numbers[self.pos - 1] {
			Some(Some(self.pos - 1)) //needed because the add and remove functions accept collections with iterators of Option<x> and not x
		} else {
			self.next()
		}
	}
}

impl IntoIterator for SudokuNumberSet {
	type Item = Option<usize>;
	type IntoIter = SudokuNumberSetIterator;

	fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
		SudokuNumberSetIterator {
			set: self,
			pos: 0,
		}
	}
}

impl std::fmt::Display for SudokuNumberSet {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "SudokuNumberSet{{ ").unwrap();
		for i in 0..9 {
			if self.numbers[i] {
				write!(f, "{}, ", i as i32 + 1).unwrap();
			}
		}
		write!(f, "}}")
	}
}

impl<T> std::ops::AddAssign<T> for SudokuNumberSet
where
	T: IntoIterator<Item = Option<usize>>
{
	fn add_assign(&mut self, rhs: T) {
		for i in rhs {
			if let Some(item) = i {
				self.numbers[item] = true;
			}
		}
	}
}

impl<T> std::ops::SubAssign<T> for SudokuNumberSet
where
	T: IntoIterator<Item = Option<usize>>
{
	fn sub_assign(&mut self, rhs: T) {
		for i in rhs {
			if let Some(item) = i {
				self.numbers[item] = false;
			}
		}
	}
}

impl<T> std::ops::Sub<T> for SudokuNumberSet
where
	T: IntoIterator<Item = Option<usize>>
{
	type Output = Self;
	fn sub(self, rhs: T) -> <Self as std::ops::Sub>::Output {
		let mut set = self;
		set -= rhs;
		set
	}
}

impl<T> std::ops::Add<T> for SudokuNumberSet
where
	T: IntoIterator<Item = Option<usize>>
{
	type Output = Self;
	fn add(self, rhs: T) -> <Self as std::ops::Sub>::Output {
		let mut set = self;
		set += rhs;
		set
	}
}

mod position {
	pub fn get_list_of_pos_in_box(box_pos: usize) -> [usize; 9] {
		let r = (box_pos / 3) * 3;
		let c = (box_pos % 3) * 3;
		let b0 = r * 9 + c;
		let b1 = b0 + 9;
		let b2 = b1 + 9;

		[
			b0 + 0, b0 + 1, b0 + 2,
			b1 + 0, b1 + 1, b1 + 2,
			b2 + 0, b2 + 1, b2 + 2,
		]
	}

	pub fn get_box_from_field(pos: usize) -> usize {
		let pos = pos / 3;
		let r = pos / 9;
		let c = pos % 3;
		r * 3 + c
	}

	pub fn print(pos: usize) -> String {
		format_args!("({}, {})", pos % 9, pos / 9).to_string()
	}
}

struct Sudoku {
	content: [Option<usize>; 81],
	hints: [Option<SudokuNumberSet>; 81],
	human_suggestions: Option<Vec<(usize, usize, usize)>>,
	description: String,
}

impl Sudoku {
	fn new(content: [Option<usize>; 81]) -> Self {
		Sudoku {
			content,
			hints: [None; 81],
			human_suggestions: None,
			description: String::new(),
		}
	}

	fn new_empty() -> Self {
		Self::new([None; 81])
	}

	fn set(&mut self, pos: usize, number: usize) {
		match self.content[pos] {
			None => {
				self.content[pos] = Some(number);
				self.hints[pos] = None;
			},
			Some(x) => panic!("already a number at pos {}: {}", position::print(pos), x),
		}
	}

	fn add_suggestion(&mut self, x: usize, y: usize, number: usize) {
		match self.human_suggestions.as_mut() {
			None => {
				self.human_suggestions = Some(Vec::new());
				self.add_suggestion(x, y, number);
			},
			Some(vec) => {
				vec.push((x, y, number));
			}
		}
	}

	fn get_row(&self, pos: usize) -> [Option<usize>; 9] {
		let mut row = [None; 9];
		for i in 0..9 {
			row[i] = self.content[(pos / 9) * 9 + i];
		}
		row
	}

	fn get_column(&self, pos: usize) -> [Option<usize>; 9] {
		let mut column = [None; 9];
		for i in 0..9 {
			column[i] = self.content[i * 9 + pos % 9];
		}
		column
	}

	fn get_box(&self, pos: usize) -> [Option<usize>; 9] {
		let box_pos = position::get_list_of_pos_in_box(position::get_box_from_field(pos));
		let mut content = [None; 9];
		for i in 0..9 {
			content[i] = self.content[box_pos[i]];
		}
		content
	}

	fn remove_numbers_from_hints_forced_by_hints(&mut self, pos: usize) {
		//TODO: make this thing only act if needed, when the update hints doesn't know what to do anymore, also log the uses of it
		let mut pos_hints = self.hints[pos].unwrap();

		// also take care of numbers not yet written, but definitively in the line
		// if a number's occurrences (hints) in a box are all within my colliding range, I can't be that number. Ignore the box I'm on

		fn get_boxes_with_pot_blocking_numbers(pos: usize) -> Vec<usize> {
			let box_pos = position::get_box_from_field(pos);

			let r = box_pos / 3;
			let c = box_pos % 3;

			[
				3 * r + 0, 3 * r + 1, 3 * r + 2,
				3 * 0 + c, 3 * 1 + c, 3 * 2 + c,
			].into_iter().filter(|&x: &usize| -> bool {
				x != box_pos
			}).collect()
		}

		for box_pos in get_boxes_with_pot_blocking_numbers(pos) {
			let mut list_of_number_positions = [Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new()];

			position::get_list_of_pos_in_box(box_pos)
				.into_iter()
				.filter(|&pos| self.content[pos].is_none())
				//.filter_map(|pos| match self.content[pos] {
				//		None => Some(pos),
				//		Some(_) => None,
				//})
				.filter_map(|pos| self.hints[pos].and_then(|set| Some((pos, set))))
				.for_each(|(pos, set): (usize, SudokuNumberSet)|
					for i in set {
						list_of_number_positions[i.unwrap()].push(pos);
						// this unwrap is needed because the iterator of SudokuNumberSet returns Option<x> and not x, because of add and remove of SudokuNumberSet
					}
				);

			list_of_number_positions.into_iter().enumerate().for_each(|(number, list)| {//TODO: beautify further
				let mut mode = None;

				if list.len() > 0 && list.into_iter().map(|position| {
					let row = pos / 9 == position / 9;
					let column = pos % 9 == position % 9;
					assert!(!(row && column), "your position must only match on either the row or column, not both");

					(row || column, row)
				}).all(|(x, m)| {
					if let None = mode {
						mode = Some(m);
					}
					x && Some(m) == mode
				}) {
					pos_hints.numbers[number] = false;
				};

				/*if list.len() > 0 && list.into_iter().all(|position| -> bool {
					let (check, c_mode) = {
						let t_row = pos / 9 == position / 9;
						let t_column = pos % 9 == position % 9;

						let at_least_one = t_row || t_column;
						assert!(!(t_row && t_column), "your position must only match on either the row or column, not both");

						(at_least_one, t_row)
					};
					if check {
						match mode {
							None => mode = Some(c_mode),
							Some(o_mode) => if c_mode != o_mode {
								return false;
							}
						}
					} else {
						return false;
					}
					return true;
				}) {
					pos_hints.numbers[number] = false;
				};*/
			});
		}
		self.set_hint(pos, Some(pos_hints));
	}

	fn set_hint(&mut self, pos: usize, hint: Option<SudokuNumberSet>) {
		if let Some(_) = self.content[pos] {
			panic!("you should only call set_hint on positions with no set number!, seems like pos {} contains something.", position::print(pos));
		}
		self.hints[pos] = hint;
	}

	fn finished(&self) -> bool {
		self.content.iter().all(|number: &Option<usize>| -> bool {
			number.is_some()
		})
	}

	fn update_hints(&mut self, advanced: bool) {
		for pos in 0..81 {
			if let None = self.content[pos] {
				let mut set = SudokuNumberSet::new_all();

				set -= self.get_row(pos);
				set -= self.get_column(pos);
				set -= self.get_box(pos);

				self.set_hint(pos, Some(set));

				if set.count_true() == 0 {
					panic!("not solvable! pos: {}, no option remains to be placed there.", position::print(pos));
				}
			}
		}
		if advanced {
			for pos in 0..81 {
				if let None = self.content[pos] {
					self.remove_numbers_from_hints_forced_by_hints(pos);
				}
			}
		}
	}

	fn place_one_position_in_box(&mut self, algorithm: &mut SudokuAlgorithm) -> bool {
		for pos in 0..81 {
			if let None = self.content[pos] {
				let mut box_set = SudokuNumberSet::new_all();
				box_set -= self.get_box(pos);

				position::get_list_of_pos_in_box(position::get_box_from_field(pos))
					.into_iter()
					.filter(|&x| self.content[x].is_none())
					.filter(|&x| pos != x)
					.filter_map(|x| self.hints[x])
					.for_each(|s| box_set -= s);/*
					//	match self.content[x] {
					//		Some(_) => false,
					//		None => pos != x,
					//	}})
					.for_each(|x: usize| {
						if let Some(item) = self.hints[x] {
							box_set -= item;
						}
					});*/

				if let Some(number) = box_set.get_one() {
					self.set(pos, number);
					algorithm.place(pos, number, "one position in box");
					return true;
				}
			}
		}
		false
	}

	fn place_one_hint(&mut self, algorithm: &mut SudokuAlgorithm) -> bool {
		for pos in 0..81 {
			if let None = self.content[pos] {
				if let Some(hints) = self.hints[pos] {
					if let Some(number) = hints.get_one() {
						self.set(pos, number);
						algorithm.place(pos, number, "one hint");
						return true;
					}
				}
			}
		}
		false
	}

	fn human_setter(&mut self, algorithm: &mut SudokuAlgorithm) -> bool {
		if let Some(suggestions) = self.human_suggestions.as_ref() {
			let suggestions = suggestions.clone();
			for (x, y, number) in suggestions {
				let pos = y * 9 + x;
				if let None = self.content[pos] {
					self.set(pos, number);
					algorithm.place(pos, number, "human setter");
					return true;
				}
			}
		}
		println!("human called: add more suggestions:\n{:?}", self);
		false
	}

	fn verify(&self) {
		for i in 0..9 {
			let pos = 9 * i + i;

			let mut column_set = SudokuNumberSet::new();
			let mut row_set = SudokuNumberSet::new();

			column_set += self.get_column(pos);
			row_set += self.get_row(pos);

			assert!(column_set.contains_all(), "column {} doesn't contain all numbers! wrong!: missing set is: {}", i, column_set.invert());
			assert!(row_set.contains_all(), "row {} doesn't contain all numbers! wrong! missing set is: {}", i, row_set.invert());
		}

		for pos in 0..9 {
			let mut box_set = SudokuNumberSet::new();

			box_set += self.get_box(pos);

			assert!(box_set.contains_all(), "box {} doesn't contain all numbers! wrong! missing set is: {}", pos, box_set.invert());
		}
	}

	fn solve_core(&mut self, algorithms: &mut [SudokuAlgorithm], print_every_round: bool) {
		let mut placed_at_least_one_number = true;
		let mut advanced = false;
		while placed_at_least_one_number && !self.finished() {
			placed_at_least_one_number = false;

			let mut last_algo = None::<usize>;
			for algo in &mut *algorithms {
				if !advanced && algo.needs_advanced_hints {
					advanced = true;
					algorithms[last_algo.unwrap()].stats.advanced_turned_on += 1;
					placed_at_least_one_number = true;
					break;
				}

				last_algo = match last_algo {
					None => Some(0),
					Some(x) => Some(x + 1),
				};

				self.update_hints(advanced);
				placed_at_least_one_number |= (algo.function)(self, algo);
				algo.stats.called += 1;
				if placed_at_least_one_number {
					algo.stats.successful += 1;
					if advanced {
						advanced = false;
						algo.stats.advanced_turned_off += 1;
					}
					break;
				}
			}

			if placed_at_least_one_number && print_every_round {
				println!("{:?}", self);
			}
		}
	}

	fn solve(&mut self, print_places: bool, print_every_round: bool, print_long: bool) -> (bool, f64) {
		macro_rules! algo {
			($function:expr, $advanced_hints:expr) => {
				SudokuAlgorithm {
					function: $function,
					printer_settings: PrinterSettings {
						places: print_places,
					},
					stats: SudokuAlgorithmStats::default(),
					name: stringify!($function).to_string(),
					needs_advanced_hints: $advanced_hints,
				}
			};
			($function:expr) => {
				algo!($function, false)
			};
		}

		/*
		TODO: add algorithm for outer and inner loop, using this:
			go create a list of stuff on the outer and inner ring,
			of a value position pair, then match up the values on the outside
			with the inside, maybe using one one side another storage for the position of
			the other field
			filter the paired ones out
			then look where in the positions (of the other side) the number may go,
			if there's only one place, place it there,
			maybe restrict the set for the others.
			maybe if only one pair exists, restrict the sets from both sides with and of them, set that as the set for both

			inner loop:
			27_|___|_36
			14_|___|_89
			__8|671|2__
			---+---+---
			__7|___|6__
			__2|___|9__
			__4|___|3__
			---+---+---
			__1|265|4__
			56_|___|_21
			42_|___|_67

			outer loop:
			2__|849|__6
			_46|___|78_
			_98|___|25_
			---+---+---
			8__|___|__2
			6__|___|__5
			9__|___|__8
			---+---+---
			_81|___|49_
			_63|___|82_
			4__|138|__7

			_ ___ _ _xx x_x _ xxx _
			__x_ 7___ 3_79 _x8_
		*/

		// TODO: maybe create a function called len() or size() in the Set, rename the count_true to it.


		// TODO: just randomly try it, then if it works continue, keep backtracking in mind...

		if print_long {
			println!("starting\n{:#?}", self);
		}

		let mut algorithms = [
			algo!(Sudoku::place_one_hint),
			algo!(Sudoku::place_one_position_in_box),
			algo!(Sudoku::human_setter, true),
		];
		{
			let max_len = algorithms.iter().map(|x: &SudokuAlgorithm| -> usize {
				x.name.len()
			}).max().unwrap();
			algorithms.iter_mut().for_each(|x: &mut SudokuAlgorithm| {
				let to_fill_up = max_len - x.name.len();
				for _ in 0..to_fill_up {
					x.name.push(' ');
				}
			});
		}

		let start = std::time::Instant::now();

		self.solve_core(&mut algorithms, print_every_round);

		let time_elapsed = start.elapsed();

		let time_needed = time_elapsed.as_nanos() as f64 / 1000_000.0;

		println!("stats: took {:.6}ms to solve {}", time_needed, self.description);
		for algo in algorithms {
			if algo.stats.should_print() {
				println!("{} -> {}", algo.name, algo.stats);
			}
		}

		let solved = self.finished();
		if solved {
			self.verify();
			print!("verify said it's solved:");
			if print_long {
				println!("\n{:#?}", self);
			} else {
				println!(" {}", self);
			}
		} else {
			println!("you need to add another algorithm!\n{:#?}", self);
		}
		println!();

		(solved, time_needed)
	}
}

impl PartialEq for Sudoku {
	fn eq(&self, other: &Self) -> bool {
		self.content == other.content
	}
}

impl From<&str> for Sudoku {
	fn from(string: &str) -> Sudoku {
		String::from(string).into()
	}
}

impl From<String> for Sudoku {
	fn from(string: String) -> Sudoku {
		#[derive(Debug, PartialEq)]
		enum ParsingMode {
			Numbers,
			Comment(char),
			Description(char),
			Suggestions(char),
		}
		use ParsingMode::*;

		let mut sudoku = Sudoku::new_empty();
		let mut pos = 0;

		let mut suggestion_pos = 0;
		let mut current_suggestion = [0; 3];

		let mut parsing_mode = Numbers;

		for i in string.chars() {
			match parsing_mode {
				Numbers => {
					let number = match i {
						'_' | '0' => None,
						'1'..='9' => Some(i as usize - 0x30 - 1),
						_ => {
							match i {
								'(' => parsing_mode = Comment(')'),
								'[' => {
									pos -= 1;
									parsing_mode = Comment(']');
								},
								'<' => parsing_mode = Description('>'),
								'{' => parsing_mode = Suggestions('}'),
								_ => (),
							};
							continue;
						}
					};
					sudoku.content[pos] = number;
					pos += 1;
				},
				Comment(x) => {
					if i == x {
						parsing_mode = Numbers;
					}
				},
				Description(x) => {
					if i == x {
						parsing_mode = Numbers;
						continue;
					}
					sudoku.description.push(i);
				},
				Suggestions(x) => {
					match i {
						'0'..='9' => {
							current_suggestion[suggestion_pos] = i as usize - 0x30;
							suggestion_pos = (suggestion_pos + 1) % 3;

							if suggestion_pos == 0 {
								sudoku.add_suggestion(current_suggestion[0], current_suggestion[1], current_suggestion[2] - 1);
							}
						}
						_ => if i == x {
							parsing_mode = Numbers;
						},
					}
				},
			}
		}

		assert_eq!(suggestion_pos, 0, "your suggestions must be a multiple of 3, {} isn't 3 * n", suggestion_pos);
		assert_eq!(parsing_mode, Numbers, "you must close all comment, descriptions or suggestions. currently the parser thinks you're in a {:?}", parsing_mode);
		assert_eq!(pos, 81, "your sudoku length isn't 81, it's {}", pos);
		sudoku
	}
}

impl std::fmt::Display for Sudoku {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for i in 0..81 {
			if i % 27 == 0 && i != 0 {
				write!(f, ",").unwrap();
			}
			write!(f, "{}", match self.content[i] {
				Some(x) => x + 1,
				None => 0,
			}).unwrap();
		}
		let mut space_written = false;
		if self.description.len() != 0 {
			write!(f, " ").unwrap();
			space_written = true;
			write!(f, "<{}>", self.description).unwrap();
		}
		if let Some(suggestions) = &self.human_suggestions {
			if suggestions.len() != 0 {
				if !space_written {
					write!(f, " ").unwrap();
				}
				write!(f, "{{").unwrap();
				for (x, y, n) in suggestions {
					write!(f, "{}{}{}", x, y, n + 1).unwrap();
				}
				write!(f, "}}").unwrap();
			}
		}
		Ok(())
	}
}

impl std::fmt::Debug for Sudoku {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "0[1 2 . 3 4 5 . 6 7 8] <{}>\n", self.description).unwrap();
		for i in 0..9 {
			if i % 3 == 0 && i != 0 {
				write!(f, "------+-------+------\n").unwrap();
			}
			for j in 0..9 {
				if j % 3 == 0 && j != 0 {
					write!(f, "| ").unwrap();
				}
				if let Some(number) = self.content[i * 9 + j] {
					write!(f, "{}", number + 1).unwrap();
				} else {
					write!(f, "_").unwrap()
				}
				write!(f, " ").unwrap();
			}
			write!(f, "({} {:2})", i, i * 9).unwrap();
			if i != 8 {
				write!(f, "\n").unwrap();
			}
		}
		if f.alternate() {
			if let Some(suggestions) = self.human_suggestions.as_ref() {
				write!(f, "\n'-> Suggestions: {{ ").unwrap();
				for (x, y, number) in suggestions {
					write!(f, "({}, {}) -> {}; ", x, y, number + 1).unwrap();
				}
				write!(f, "}}").unwrap();
			}
		}
		Ok(())
	}
}

#[derive(Debug)]
struct SudokuAlgorithmStats {
	called: i32,
	successful: i32,
	advanced_turned_on: i32,
	advanced_turned_off: i32,
}

impl SudokuAlgorithmStats {
	fn should_print(&self) -> bool {
		self.called != 0 || self.successful != 0 || self.advanced_turned_on != 0 || self.advanced_turned_off != 0
	}
}

impl Default for SudokuAlgorithmStats {
	fn default() -> Self {
		SudokuAlgorithmStats {
			called: 0,
			successful: 0,
			advanced_turned_on: 0,
			advanced_turned_off: 0,
		}
	}
}

impl std::fmt::Display for SudokuAlgorithmStats {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Stats: {{ ").unwrap();
		write!(f, "called: {:3}, ", self.called).unwrap();
		write!(f, "successes: {:3}, ", self.successful).unwrap();
		write!(f, "turned on advanced: {:3}, ", self.advanced_turned_on).unwrap();
		write!(f, "turned off advanced: {:3} ", self.advanced_turned_off).unwrap();
		write!(f, "}}").unwrap();
		Ok(())
	}
}

struct PrinterSettings {
	places: bool
}

struct SudokuAlgorithm {
	function: fn(&mut Sudoku, &mut SudokuAlgorithm) -> bool,
	stats: SudokuAlgorithmStats,
	printer_settings: PrinterSettings,
	needs_advanced_hints: bool,
	name: String,
}

impl SudokuAlgorithm {
	fn place(&self, pos: usize, number: usize, name: &str) {
		if self.printer_settings.places {
			println!("{} -> {} | {}", position::print(pos), number + 1, name)
		}
	}
}

fn main() {
	// format:
	// <27 sudoku digits>,<27 sudoku digits>,<27 sudoku digits>(<char><number>;<sub>)
	// - char: indicates the source of the sudoku:
	//  S = Stuttgarter Zeitung,
	//  J = Jeux,
	//  F = _das ding vom papa_,
	// - number: the number that is written on the original, leading zeroes are replaced with _, the number is 3 digits long.
	// - sub: the sub description, if not used (char = F) it's _. this is one char in length.
	// sort alphabetically, only with the part in brackets. also note the tab after =.
	//
	let sudokus = [
	//	"900380100000005007007006300,651900430070000205040800000,090700020403009071000003906 <F__0;_>", // solvable
	//	"021000430000905000750020096,004702300200000001009801600,360010084000508000085000720 <J__0;1>", // solvable
	//	"003000600025408390970000054,000732000000080000000591000,560000032094205780001000900 <J__0;2>", // solvable
	//	"090000070002107600000843000,058070460004000200067020980,000216000003904800020000010 <J__0;3>", // solvable
	//	"050701090009000300080509040,043000920600000004097000180,070908060002000400060304070 <J__0;4>",
	//	"060000080100020003300801009,007203900000080000003105200,700506004500040006020000070 <J__0;5>", // solvable
	//	"010000030045010280900207001,009504800000000000004109600,300406002098050170020000040 <J__0;6>",
	//	"480000027005102800900000003,007301900300000001009608300,700000008008706400210000035 <J__0;7>",
	//	"301000208026401730500000004,000102000060040050000603000,100000006079506840604000901 <J__0;8>",
	//	"005809100040302080300000004,007913600000000000004526300,700000003060407020009108500 <J__0;9>{228}",
		"500260000008140000040509700,000820056600000030002300000,004000097800000015060002480 <J__1;1>",
	//	"000012000001000200420000086,009000500230000074000038000,900000007080461090050379010 <J__1;2>", // solvable
	//	"070004090009000004800060000,008002070017000205400008900,300007060005200030780310000 <J__1;3>",
	//	"070000060492008003050090000,000000080060024009009150047,030001095500076320000040000 <J__1;4>", // solvable
	//	"400100509010020000600400073,000000008020008005930000260,000260790000000001004090002 <J__1;5>", // solvable
	//	"000700060460001300001090002,005080006310006400000400050,120000000000305270000802910 <J__1;6>",
	//	"350090001000005400642000000,423000000580040003000003700,000900080000072304000051209 <J__1;7>",
	//	"000000000805190000073406005,000000000014209670027508390,901680000056907003000000000 <J__1;8>",
	//	"096370021100020004000004800,057030006014080002000000000,043610087700050009000002300 <J__1;9>",
	//	"702003500500640000098010000,023000060010080050050000790,000050640000038005005700903 <S__0;M>", // solvable
	//	"480001000905020000000000070,000037801007040600304580000,020000000000070308000300054 <S__0;S>", // solvable
	//	"930000610600804000870000900,500900200000132000009007001,005000067000201004048000052 <S__1;M>", // solvable
	//	"000230000002900003095000100,030000400200413006006000050,001000970400002800000079000 <S__1;S>", // solvable
	//	"905000060006705200004360700,000800020500030008060004000,009043100003901800020000906 <S__2;M>", // solvable
	//	"050000602007000900009002008,000040001070168050200050000,300400700002000300905000020 <S__2;S>", // solvable
	//	"730000920500802000940000600,200400500000267000006008003,004000032000105009079000051 <S__3;M>", // solvable
	//	"000801003005000084076009000,000100070400030005080002000,000900820920000100600204000 <S__3;S>", // solvable
	//	"000590000500048093000600400,320000809006020700704000012,001007000290150006000089000 <S__4;M>", // solvable
	//	"800002500050000070000705030,002050060900060002070010300,030104000020000040006900008 <S__4;S>", // solvable
	];

	let mut solve_times = Vec::with_capacity(sudokus.len());

	for sudoku in sudokus {
		let mut sudoku: Sudoku = sudoku.into();
		let (solved, time) = sudoku.solve(false, false, false);
		if solved {
			solve_times.push(time);
		}
	}

	if solve_times.len() != 0 {
		let average = solve_times.iter().sum::<f64>() / solve_times.len() as f64;
		println!("average time of solves: {:.6}ms", average);
	}
}
