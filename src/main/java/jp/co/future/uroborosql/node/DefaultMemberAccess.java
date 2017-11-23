/**
 *
 */
package jp.co.future.uroborosql.node;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Member;
import java.lang.reflect.Modifier;
import java.util.Map;

import ognl.MemberAccess;

/**
 * MemberAccess実装クラス
 *
 * @author H.Sugimoto
 *
 */
public class DefaultMemberAccess implements MemberAccess {

	private boolean allowPrivateAccess = false;
	private boolean allowProtectedAccess = false;
	private boolean allowPackageProtectedAccess = false;

	/**
	 * コンストラクタ
	 *
	 * @param allowAllAccess private/protected/package スコープのアクセス可否
	 */
	public DefaultMemberAccess(final boolean allowAllAccess) {
		this(allowAllAccess, allowAllAccess, allowAllAccess);
	}

	/**
	 * コンストラクタ
	 *
	 * @param allowPrivateAccess privateスコープのアクセス可否
	 * @param allowProtectedAccess protectedスコープのアクセス可否
	 * @param allowPackageProtectedAccess packageスコープのアクセス可否
	 */
	public DefaultMemberAccess(final boolean allowPrivateAccess, final boolean allowProtectedAccess,
			final boolean allowPackageProtectedAccess) {
		super();
		this.allowPrivateAccess = allowPrivateAccess;
		this.allowProtectedAccess = allowProtectedAccess;
		this.allowPackageProtectedAccess = allowPackageProtectedAccess;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.MemberAccess#setup(java.util.Map, java.lang.Object, java.lang.reflect.Member, java.lang.String)
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public Object setup(final Map context, final Object target, final Member member, final String propertyName) {
		Object result = null;

		if (isAccessible(context, target, member, propertyName)) {
			AccessibleObject accessible = (AccessibleObject) member;

			if (!accessible.isAccessible()) {
				result = Boolean.TRUE;
				accessible.setAccessible(true);
			}
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.MemberAccess#restore(java.util.Map, java.lang.Object, java.lang.reflect.Member, java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public void restore(final Map context, final Object target, final Member member, final String propertyName,
			final Object state) {
		if (state != null) {
			((AccessibleObject) member).setAccessible(((Boolean) state).booleanValue());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see ognl.MemberAccess#isAccessible(java.util.Map, java.lang.Object, java.lang.reflect.Member, java.lang.String)
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public boolean isAccessible(final Map context, final Object target, final Member member,
			final String propertyName) {
		int modifiers = member.getModifiers();
		boolean result = Modifier.isPublic(modifiers);

		if (!result) {
			if (Modifier.isPrivate(modifiers)) {
				result = allowPrivateAccess;
			} else {
				if (Modifier.isProtected(modifiers)) {
					result = allowProtectedAccess;
				} else {
					result = allowPackageProtectedAccess;
				}
			}
		}
		return result;
	}
}
