import * as React from 'react';

export const ClientSideRender = (props: { children: JSX.Element } ) => {
	return process.browser ? (
		props.children
	) : null
}