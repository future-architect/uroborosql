package jp.co.future.uroborosql.coverage;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Range情報
 *
 * @author ota
 */
public class Ranges extends AbstractCollection<Range> {

	private final List<Range> ranges = new LinkedList<>();

	/**
	 * コンストラクタ
	 */
	public Ranges() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param ranges 初期データ
	 */
	public Ranges(Collection<? extends Range> ranges) {
		addAll(ranges);
	}

	@Override
	public Iterator<Range> iterator() {
		ranges.sort(Comparator.comparingInt(Range::getStart).thenComparingInt(Range::getEnd));
		return ranges.iterator();
	}

	@Override
	public boolean add(Range range) {
		Range target = range;
		for (Iterator<Range> iterator = ranges.iterator(); iterator.hasNext();) {
			Range r = iterator.next();
			if (r.equals(target)) {
				return false;
			} else if (r.include(target)) {
				return false;
			} else if (target.getEnd() + 1 == r.getStart()) {
				//吸収して元を削除
				target = new Range(target.getStart(), r.getEnd());
				iterator.remove();
			} else if (r.getEnd() + 1 == target.getStart()) {
				//吸収して元を削除
				target = new Range(r.getStart(), target.getEnd());
				iterator.remove();
			} else if (r.intersection(target)) {
				//吸収して元を削除 本機能では通常通らないはず
				target = new Range(Math.min(r.getStart(), target.getStart()), Math.max(r.getEnd(), target.getEnd()));
				iterator.remove();
			}
		}
		ranges.add(target);
		return true;
	}

	@Override
	public String toString() {
		return ranges.toString();
	}

	@Override
	public int size() {
		return ranges.size();
	}

}
