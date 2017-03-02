package jp.co.future.uroborosql.testlog;

import java.nio.charset.Charset;

import ch.qos.logback.core.Appender;
import ch.qos.logback.core.CoreConstants;
import ch.qos.logback.core.Layout;
import ch.qos.logback.core.UnsynchronizedAppenderBase;
import ch.qos.logback.core.encoder.Encoder;
import ch.qos.logback.core.encoder.LayoutWrappingEncoder;
import ch.qos.logback.core.spi.DeferredProcessingAware;
import ch.qos.logback.core.status.ErrorStatus;

/**
 * エンコード結果を処理する{@link Appender} のAbstractクラス
 *
 * @param <E> Encoder event type
 * @author ota
 */
public abstract class AbstractEncodedAppender<E> extends UnsynchronizedAppenderBase<E> {

	private Encoder<E> encoder;

	/**
	 * コンストラクタ
	 */
	public AbstractEncodedAppender() {
	}

	/**
	 * Encoderの取得
	 *
	 * @return Encoder
	 */
	public Encoder<E> getEncoder() {
		return this.encoder;
	}

	/**
	 * Encoderの設定<br>
	 * 通常はxmlから指定する
	 *
	 * @param encoder {@code <encoder>}
	 */
	public void setEncoder(Encoder<E> encoder) {
		this.encoder = encoder;
	}

	/**
	 * レイアウトの設定<br>
	 * 通常は {@link #setEncoder(Encoder)}を利用する
	 *
	 * @param layout {@code <layout>}
	 */
	public void setLayout(Layout<E> layout) {
		LayoutWrappingEncoder<E> lwe = new LayoutWrappingEncoder<>();
		lwe.setLayout(layout);
		lwe.setContext(this.context);
		this.encoder = lwe;
	}

	@Override
	public void start() {
		if (this.encoder == null) {
			addStatus(new ErrorStatus("No encoder set for the appender named \"" + this.name + "\".", this));
			return;
		}

		super.start();
	}

	@Override
	protected final void append(E eventObject) {
		subAppend(eventObject);
	}

	/**
	 * エンコード済のログの書き込み
	 *
	 * @param encodedLog エンコード済のログ
	 */
	protected abstract void append(byte[] encodedLog);

	/**
	 * ログbyte配列のの文字列変換
	 *
	 * @param encodedLog ログbyte配列
	 * @return 変換後文字列
	 */
	protected String toStringLog(byte[] encodedLog) {
		String s = new String(encodedLog, getCharset());
		if (s.endsWith(CoreConstants.LINE_SEPARATOR)) {
			s = s.substring(0, s.length() - CoreConstants.LINE_SEPARATOR.length());
		}
		return s;
	}

	/**
	 * Charset取得 （取れる範囲で）
	 *
	 * @return Charset
	 */
	protected Charset getCharset() {

		Charset charset = null;
		if (this.encoder instanceof LayoutWrappingEncoder) {
			charset = ((LayoutWrappingEncoder<?>) this.encoder).getCharset();
		}
		return charset != null ? charset : Charset.defaultCharset();

	}

	@Override
	public void stop() {
		super.stop();
	}



	private void subAppend(E event) {
		if (event instanceof DeferredProcessingAware) {
			((DeferredProcessingAware) event).prepareForDeferredProcessing();
		}
		append(this.encoder.encode(event));
	}

}