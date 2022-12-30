package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

/**
 * ConcurrentLruCacheのテストケース
 *
 * @author H.Sugimoto
 */
public class ConcurrentLruCacheTest {

	/**
	 * キャッシュが有効な場合のテストケース
	 *
	 * @throws Exception 例外発生時
	 */
	@Test
	public void testCache() throws Exception {
		ConcurrentLruCache<String, Integer> cache = new ConcurrentLruCache<>(3);
		AtomicInteger counter = new AtomicInteger();

		// 値の追加
		assertThat(cache.get("key1", key -> counter.incrementAndGet()), is(1));
		assertThat(cache.size(), is(1));
		assertThat(cache.sizeLimit(), is(3));

		// キャッシュからの値の取得
		assertThat(cache.get("key1", key -> counter.incrementAndGet()), is(1));
		assertThat(cache.size(), is(1));
		assertThat(cache.sizeLimit(), is(3));

		// 値の追加（sizeLimit内）
		assertThat(cache.get("key2", key -> counter.incrementAndGet()), is(2));
		assertThat(cache.size(), is(2));
		assertThat(cache.sizeLimit(), is(3));

		// 値の追加（sizeLimit上限）
		assertThat(cache.get("key3", key -> counter.incrementAndGet()), is(3));
		assertThat(cache.size(), is(3));
		assertThat(cache.sizeLimit(), is(3));

		// 値の追加（sizeLimitオーバー）
		assertThat(cache.get("key4", key -> counter.incrementAndGet()), is(4));
		assertThat(cache.size(), is(3));
		assertThat(cache.sizeLimit(), is(3));

		// 値の追加（sizeLimitでのキャッシュからの取得）
		assertThat(cache.get("key4", key -> counter.incrementAndGet()), is(4));
		assertThat(cache.size(), is(3));
		assertThat(cache.sizeLimit(), is(3));

		// sizeLimitあふれにより最初の要素がキャッシュから削除されていること
		assertThat(cache.contains("key1"), is(false));
		assertThat(cache.contains("key2"), is(true));
		assertThat(cache.contains("key3"), is(true));
		assertThat(cache.contains("key4"), is(true));

		// 存在しないキーの削除
		assertThat(cache.remove("key1"), is(false));
		// 存在するキーの削除
		assertThat(cache.remove("key2"), is(true));
		assertThat(cache.size(), is(2));

		// キャッシュのクリア
		cache.clear();
		assertThat(cache.size(), is(0));
	}

	/**
	 * キャッシュが無効な場合のテストケース
	 *
	 * @throws Exception 例外発生時
	 */
	@Test
	public void testNoCache() throws Exception {
		ConcurrentLruCache<String, Integer> cache = new ConcurrentLruCache<>(0);
		AtomicInteger counter = new AtomicInteger();

		// 値の追加
		assertThat(cache.get("key1", key -> counter.incrementAndGet()), is(1));
		assertThat(cache.size(), is(0));
		assertThat(cache.sizeLimit(), is(0));

		// キャッシュからの値の取得
		assertThat(cache.get("key1", key -> counter.incrementAndGet()), is(2));
		assertThat(cache.size(), is(0));
		assertThat(cache.sizeLimit(), is(0));
	}

	/**
	 * limitSizeが不正な場合のテストケース
	 *
	 * @throws Exception 例外発生時
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalCache() throws Exception {
		new ConcurrentLruCache<>(-1);
	}

	/**
	 * 値取得時のgeneratorが指定されていない場合のテストケース
	 *
	 * @throws Exception 例外発生時
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalGenerator() throws NoSuchFieldException, SecurityException {
		ConcurrentLruCache<String, Integer> cache = new ConcurrentLruCache<>(3);
		// generatorの指定なし
		cache.get("key1", null);
	}
}
