namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Library =
    let onImportBindings cont transformFn =
        function
        | Ok bindings -> transformFn bindings |> cont
        | x -> x |> cont

    let resolveImportOnlyId bindings id pos =
        match bindings |> Map.tryFind id with
        | Some r -> Ok(Some(id, r))
        | None -> EvalError(sprintf "only: identifier '%s' not exported." id, pos) |> Error

    let collectImportOnlyIds bindings ids =
        ids
        |> mapResult (function
            | SSymbol id, pos -> resolveImportOnlyId bindings id pos
            | x -> EvalError("only: identifier expected.", snd x) |> Error)
        |> Result.map (fun pairs -> pairs |> List.choose id |> Map.ofList)

    let processImportSetOnly cont ids =
        onImportBindings cont (fun bindings ->
            ids
            |> toList
            |> Result.bind (fun idList -> idList |> collectImportOnlyIds bindings))

    let resolveImportExceptId bindings id pos =
        if bindings |> Map.containsKey id then
            Ok id
        else
            EvalError(sprintf "except: identifier '%s' not exported." id, pos) |> Error

    let collectImportExceptIds bindings ids =
        ids
        |> mapResult (function
            | SSymbol id, pos -> resolveImportExceptId bindings id pos |> Result.map (fun _ -> id)
            | x -> EvalError("except: identifier expected.", snd x) |> Error)

    let processImportSetExcept cont ids =
        onImportBindings cont (fun bindings ->
            ids
            |> toList
            |> Result.bind (fun idList ->
                idList
                |> collectImportExceptIds bindings
                |> Result.map (fun removedIds ->
                    removedIds |> List.fold (fun acc id -> acc |> Map.remove id) bindings)))

    let processImportSetPrefix cont prefix =
        onImportBindings cont (fun bindings ->
            bindings
            |> Map.toSeq
            |> Seq.map (fun (name, r) -> prefix + name, r)
            |> Map.ofSeq
            |> Ok)

    let resolveRenameClause bindings =
        function
        | SPair { car = SSymbol fromId, _
                  cdr = SPair { car = SSymbol toId, _
                                cdr = SEmpty, _ },
                        _ },
          pos ->
            match bindings |> Map.tryFind fromId with
            | Some r -> Ok(fromId, toId, r)
            | None -> EvalError(sprintf "rename: identifier '%s' not exported." fromId, pos) |> Error
        | x -> EvalError("rename: invalid rename clause.", snd x) |> Error

    let applyRename bindings (fromId, toId, r) =
        bindings |> Map.remove fromId |> Map.add toId r

    let processImportSetRename cont renames =
        onImportBindings cont (fun bindings ->
            renames
            |> toList
            |> Result.bind (fun renameList ->
                renameList
                |> mapResult (resolveRenameClause bindings)
                |> Result.map (fun clauses -> clauses |> List.fold applyRename bindings)))

    [<TailCall>]
    let rec processImportSet context pos cont =
        function
        | SPair { car = SSymbol "only", _
                  cdr = SPair { car = inner; cdr = ids }, _ },
          _ -> inner |> processImportSet context pos (processImportSetOnly cont ids)
        | SPair { car = SSymbol "except", _
                  cdr = SPair { car = inner; cdr = ids }, _ },
          _ -> inner |> processImportSet context pos (processImportSetExcept cont ids)
        | SPair { car = SSymbol "prefix", _
                  cdr = SPair { car = inner
                                cdr = SPair { car = SSymbol prefix, _
                                              cdr = SEmpty, _ },
                                      _ },
                        _ },
          _ -> inner |> processImportSet context pos (processImportSetPrefix cont prefix)
        | SPair { car = SSymbol "rename", _
                  cdr = SPair { car = inner; cdr = renames }, _ },
          _ -> inner |> processImportSet context pos (processImportSetRename cont renames)
        | imports ->
            match imports |> Context.lookupLibrary context pos with
            | Ok lib ->
                lib.exports
                |> Map.fold
                    (fun acc externalName internalName ->
                        match internalName |> Context.tryLookupEnvironment lib.environment with
                        | Some r -> acc |> Map.add externalName r
                        | None -> acc)
                    Map.empty
                |> Ok
                |> cont
            | Error e -> Error e |> cont

    [<TailCall>]
    let rec sImport context pos cont =
        function
        | [] -> Ok(SUnspecified, pos) |> cont
        | importSet :: rest ->
            importSet
            |> processImportSet context pos (function
                | Ok bindings ->
                    let currentEnv = context.environments.Head

                    bindings
                    |> Map.iter (fun name refVal -> currentEnv.Value <- currentEnv.Value |> Map.add name refVal)

                    rest |> sImport context pos cont
                | Error e -> Error e |> cont)

    [<TailCall>]
    let rec loopLibraryExport pos acc =
        function
        | [] -> Ok acc
        | (SSymbol name, _) :: rest -> rest |> loopLibraryExport pos (acc |> Map.add name name)
        | (SPair { car = SSymbol "rename", _
                   cdr = SPair { car = SSymbol oldName, _
                                 cdr = SPair { car = SSymbol newName, _
                                               cdr = SEmpty, _ },
                                       _ },
                         _ },
           _) :: rest -> rest |> loopLibraryExport pos (acc |> Map.add newName oldName)
        | x :: _ -> [ x ] |> invalidParameter pos "'%s' invalid export parameter."

    let processLibraryExport exports pos cont declaration =
        match declaration |> toList with
        | Ok dlist -> dlist |> loopLibraryExport pos exports |> cont
        | Error e -> Error e |> cont

    [<TailCall>]
    let rec readLibraryDeclarations pos foldCase acc =
        function
        | [] -> acc |> List.rev |> Ok
        | (SString f, fp) :: rest ->
            match tryReadAll foldCase f fp with
            | Ok expressions -> rest |> readLibraryDeclarations pos foldCase (List.rev expressions @ acc)
            | Error e -> Error e
        | x :: _ ->
            EvalError(sprintf "'%s' invalid include-library-declarations parameter." (x |> Print.print), pos)
            |> Error

    type LibDecl =
        | ImportDecl of importSets: SExpression
        | ExportDecl of exportSpecs: SExpression
        | BeginDecl of exprs: SExpression
        | IncludeDecl of files: SExpression * pos: Position option
        | IncludeCiDecl of files: SExpression * pos: Position option
        | IncludeLibDecl of files: SExpression * pos: Position option
        | CondExpandDecl of clauses: SExpression * expandPos: Position option

    let parseLibraryDeclaration =
        function
        | SPair { car = SSymbol "import", _
                  cdr = importSets },
          _ -> Ok(ImportDecl importSets)
        | SPair { car = SSymbol "export", _
                  cdr = exportSpecs },
          _ -> Ok(ExportDecl exportSpecs)
        | SPair { car = SSymbol "begin", _
                  cdr = exprs },
          _ -> Ok(BeginDecl exprs)
        | SPair { car = SSymbol "include", p
                  cdr = files },
          _ -> Ok(IncludeDecl(files, p))
        | SPair { car = SSymbol "include-ci", p
                  cdr = files },
          _ -> Ok(IncludeCiDecl(files, p))
        | SPair { car = SSymbol "include-library-declarations", p
                  cdr = files },
          _ -> Ok(IncludeLibDecl(files, p))
        | SPair { car = SSymbol "cond-expand", expandPos
                  cdr = clauses },
          _ -> Ok(CondExpandDecl(clauses, expandPos))
        | x -> x |> invalid (snd x) "'%s' invalid library declaration."

    [<TailCall>]
    let rec processLibraryDeclaration pos cont foldCase libContext exports =
        function
        | [] -> Ok exports |> cont
        | declaration :: declarations ->
            match parseLibraryDeclaration declaration with
            | Ok(ImportDecl importSets) ->
                processImportDecl pos cont foldCase libContext exports importSets declarations
            | Ok(ExportDecl exportSpecs) ->
                processExportDecl pos cont foldCase libContext exports exportSpecs declarations
            | Ok(BeginDecl exprs) -> processBeginDecl pos cont foldCase libContext exports exprs declarations
            | Ok(IncludeDecl(files, p)) -> processIncludeDecl pos cont foldCase libContext exports files p declarations
            | Ok(IncludeCiDecl(files, p)) ->
                processIncludeCiDecl pos cont foldCase libContext exports files p declarations
            | Ok(IncludeLibDecl(files, p)) ->
                processIncludeLibDecl pos cont foldCase libContext exports files p declarations
            | Ok(CondExpandDecl(clauses, expandPos)) ->
                processCondExpandDecl pos cont foldCase libContext exports clauses expandPos declarations
            | Error e -> Error e |> cont

    and processImportDecl pos cont foldCase libContext exports importSets declarations =
        match importSets |> toList with
        | Ok isets ->
            isets
            |> sImport libContext pos (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
        | Error e -> Error e |> cont

    and processExportDecl pos cont foldCase libContext exports exportSpecs declarations =
        exportSpecs
        |> processLibraryExport exports pos (function
            | Ok newExports ->
                declarations
                |> processLibraryDeclaration pos cont foldCase libContext newExports
            | Error e -> Error e |> cont)

    and processBeginDecl pos cont foldCase libContext exports exprs declarations =
        match exprs |> toList with
        | Ok elist ->
            elist
            |> Eval.eachEval
                libContext
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                (Ok(SUnspecified, pos))
        | Error e -> Error e |> cont

    and processIncludeDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            flist
            |> sIncludeFiles
                false
                libContext
                filePos
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                []
        | Error e -> Error e |> cont

    and processIncludeCiDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            flist
            |> sIncludeFiles
                true
                libContext
                filePos
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                []
        | Error e -> Error e |> cont

    and processIncludeLibDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            match flist |> readLibraryDeclarations filePos foldCase [] with
            | Ok decls ->
                decls @ declarations
                |> processLibraryDeclaration pos cont foldCase libContext exports
            | Error e -> Error e |> cont
        | Error e -> Error e |> cont

    and processCondExpandDecl pos cont foldCase libContext exports clauses expandPos declarations =
        match clauses |> toList with
        | Ok clist ->
            clist
            |> evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations
        | Error e -> Error e |> cont

    and [<TailCall>] evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations =
        function
        | [] -> EvalError("No matching clause in cond-expand.", expandPos) |> Error |> cont
        | clause :: cRest ->
            match clause with
            | SPair { car = SSymbol "else", _; cdr = body }, _ ->
                match body |> toList with
                | Ok expressions ->
                    expressions @ declarations
                    |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont
            | SPair { car = requirement; cdr = body }, _ ->
                if checkFeatureRequirement libContext pos false requirement then
                    match body |> toList with
                    | Ok expressions ->
                        expressions @ declarations
                        |> processLibraryDeclaration pos cont foldCase libContext exports
                    | Error e -> Error e |> cont
                else
                    cRest
                    |> evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations
            | x -> x |> invalid (snd x) "'%s' invalid cond-expand clause." |> cont

    let sDefineLibrary context pos cont =
        function
        | name :: declarations ->
            let libContext = [] |> Context.extendEnvironments { context with environments = [] }

            declarations
            |> processLibraryDeclaration
                pos
                (function
                | Ok exports ->
                    Context.registerLibrary context name libContext.environments.Head exports
                    Ok(SUnspecified, pos) |> cont
                | Error e -> Error e |> cont)
                false
                libContext
                Map.empty
        | x -> x |> invalidParameter pos "'%s' invalid define-library parameter." |> cont
