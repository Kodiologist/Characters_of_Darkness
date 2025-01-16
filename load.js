// This script requires Pyodide, and it gets some other files with
// `pyfetch`.

window.onload = async function()

   {let startup = async function()
       {pyodide = await loadPyodide()
        await pyodide.loadPackage('micropip')
        await pyodide.runPythonAsync(`
            import sys, ast, pyodide, micropip, js
            from pathlib import Path
            from pyodide.http import pyfetch

            # Fetch Hy and install its dependencies.
            await (await pyfetch('chargen/hy-for-pyodide.tar.gz')).unpack_archive()
            for dependency in ast.parse(Path("setup.py").read_text()).body[0].value.elts:
                 await micropip.install(dependency.value)
            import hy

            Path("characters_of_darkness.hy").write_bytes(
                await (await pyfetch('chargen/characters_of_darkness.hy')).bytes())
            import characters_of_darkness as COD
            COD.import_build(COD.initial_build)`)}

    let lm = document.getElementById('loading-message')
    lm.textContent = 'Loading…'
    try
       {await startup()
        lm.remove()}
    catch (error)
       {lm.textContent = error.toString()}}
