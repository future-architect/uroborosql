package jp.co.future.uroborosql.event;

/**
 * EventSubscriberインタフェース
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public interface EventSubscriber {

	/**
	 * 初期化処理.
	 */
	default void initialize() {
		// do nothing
	}

	/**
	 * イベントの購読を行う.
	 *
	 * @param eventListenerHolder EventListenerHolder
	 */
	void subscribe(EventListenerHolder eventListenerHolder);
}
