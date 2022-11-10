import * as React from 'react';
import AceEditor from "react-ace";

const DGDLEditor: React.VFC<{
	value: string,
	onChange: (value: string) => void
}> = ({onChange, value }) => {

	React.useEffect(() => {
		import("ace-builds/src-noconflict/mode-yaml");
		import("ace-builds/src-noconflict/theme-github");
		import("ace-builds/src-noconflict/ext-language_tools");
	}, [])

	return process.browser ? (
	  <AceEditor
	    mode="yaml"
	    value={value}
	    width="100%"
	    fontSize="18px"
	    theme="github"
	    onChange={(value) => onChange(value)}
	    name="dgdl-editor"
	    editorProps={{ $blockScrolling: true }}
	  />
	): null;
}

export default DGDLEditor