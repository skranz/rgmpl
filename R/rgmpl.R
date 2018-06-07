# Utils that facilitate solving GMPL models from R
#
# Autor: Sebastian Kranz (skranz@uni-ulm.de)
#
# Version vom 18.11.2012


#' Internal function that gets a list with sets, variables and parameters
#' of a GMPL model
#' @param mod.file path of the .mod file in which the gmpl model is specified
#' @export
gmpl.get.model.info = function(mod.file) {
  # Gets a list with sets, variables and parameters
  # of a GMPL model in the file mod.file
  
  restore.point("gmpl.get.model.info")
  
  is.mip = FALSE
  
  require(stringr)
  str = readLines(mod.file)
  # Remove whitespaces from left or right 
  str = str_trim(str)
  
  # Find sets 
  rows = str_sub(str,1,3) == "set"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  sets.name = str_sub(txt,1,end.name-1)
  
  extract.sets = function(txt) {
    brace.start = str_locate(txt,fixed("{"))[,1]
    brace.end = str_locate(txt,fixed("}"))[,1]
    in.brace = str_sub(txt,brace.start+1,brace.end-1)
    my.sets = str_split(in.brace,fixed(','))
    my.sets = lapply(my.sets,str_trim)
    my.sets = lapply(my.sets, function(set) {
      if (is.na(set[1]))
        return(set)
      set = str_trim(set)
      in.pos = str_locate(set,fixed(" in "))[,2]
      rows = which(!is.na(in.pos))
      if (length(rows)>0)
        set[rows] = substring(set[rows],in.pos[rows]+1)                
      return(set)
    })
    return(my.sets)
  }
  
  # Find sets for each set
  sets.sets = extract.sets(txt)
  names(sets.sets)=sets.name

  
  # Find variables
  rows = str_sub(str,1,3) == "var"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ,]")[,1]
  var.name = str_sub(txt,1,end.name-1)
  
  var.opts = substring(txt, end.name+1)
  is.mip = any(str_detect(var.opts, fixed("binary")) |  str_detect(var.opts, fixed("integer")))
  
  
  # Find sets for each variable
  var.sets = extract.sets(txt)
  names(var.sets)=var.name
  
  # Find parameters
  rows = str_sub(str,1,5) == "param"
  txt = str_trim(str_sub(str[rows],6))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  param.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each parameter
  param.sets = extract.sets(txt)
  names(param.sets)=param.name
  
  # Extract those parameters that are defined to be equal to some value
  comment.start = str_locate(txt,fixed("#"))[,1]
  comment.start[is.na(comment.start)] = 1000000  
  param.defined = str_detect(substring(txt,1,comment.start),fixed("="))
  
  
  return(list(sets=sets.name,sets.sets = sets.sets,
              var=var.name,var.sets=var.sets,
              param=param.name,param.sets=param.sets,
              param.defined=param.defined, is.mip = is.mip))
}  

#' Generates a GMPL data file
#' 
#' Generates a GMPL data file for the model specified in dat.file
#' sets and param are lists that contain the 
#' values of the sets and parameters that
#' are specified in the GMPL model
#' @param sets a list with the sets used by the gmpl model
#' @param param a list with the parameters used by the gmpl model
#' @param mod.file path of the .mod file in which the gmpl model is specified
#' @param dat.file path of the .dat file in which the data shall be written
#' @export
gmpl.make.dat.file =  function(sets=NULL,param=NULL,mod.file,dat.file=NULL, helper.sets = NULL) {
  # Generates a GMPL data file for the model specified in dat.file
  # sets and param are lists that contain the 
  # values of the sets and parameters that
  # are specified in the GMPL model
  
  restore.point("gmpl.make.dat.file")
  
  mi = gmpl.get.model.info(mod.file)
  
  mi$param = mi$param[!mi$param.defined]
  mi$param.sets = mi$param.sets[!mi$param.defined]
  
  if (!setequal(names(sets),mi$sets)) {
    print("Error: Model file specifies the following sets:")
    print(mi$sets)
    print("but you specified now the following sets:")
    print(names(sets))
    stop()
  }
  if (!setequal(names(param),mi$param)) {
    print("Error: Model file specifies the following parameters:")
    print(mi$param)
    print("but you specified now the following parameters:")
    print(names(param))
    stop()
  }
  
  mypaste = function(..., sep="",collapse=" ") {
    paste(...,sep=sep,collapse=collapse)
  }
  #########################################
  # write sets
  #########################################
  str = ""
  if (!is.null(sets)) {
    str = character(length(sets))
    for (i in 1:length(sets)) {
      # Set is not defined over another set
      if (is.na(mi$sets.sets[[i]])) {
        str[i] = mypaste(
          "  set ",names(sets)[i], " := ", mypaste(sets[[i]]),";\n")
      
      # Set is defined over one or more sets
      # Assume that set is a list in the right order
      } else {
        set = sets[[i]]
        set.name = names(sets)[[i]]
        set.sets = sets[mi$sets.sets[[set.name]]]
      
        
        if (length(set.sets)==1) {
          index.name = set.sets[[1]]
        } else { 
          grid = expand.grid(set.sets)
          index.name = grid[,1]
          for (j in 2:NCOL(grid)) {
            index.name = paste(index.name,grid[,j],sep=",")
          }
        }
        
        if (!is.list(set) | length(set) != NROW(index.name)) {
          stop(paste("Error: Model file specifies set ",set.name," as a set over the sets ",paste(set.sets,collapse=","),". You must therefore specify this set as a list of sets with length ", NROW(index.name), "!"))
        }          
        fun = function(ind) {
          mypaste("  set ",set.name, "[",index.name[ind],"] := ", mypaste(set[[ind]]),";\n")  
        }
        str[i] = paste(sapply(1:NROW(index.name),fun),collapse="\n")
      }
    }
    
    str = mypaste(
"
/* Sets */
",
    mypaste(str,collapse="\n"))
  }
    
  txt = str
  # write parameters
  if (!is.null(param)) {
    str = character(length(param))
    for (i in 1:length(param)) {
      p = param[[i]]
      pn = names(param)[[i]]
      set.names = mi$param.sets[[pn]]
      pset = if (!is.na(set.names)) {
        lapply(set.names, function(set.name) {
          if (set.name %in% names(sets)) return(sets[[set.name]])
          if (set.name %in% names(helper.sets)) return(helper.sets[[set.name]])
          restore.point("shfhdfjhkhk")
          if (str_detect(set.name,fixed(".."))) {
            set.name = gsub("..", ":", set.name,fixed = TRUE)
            pset = eval(parse(text=set.name), param)
            return(pset)
          }
          stop(paste0("Unknown set specification ", set.name, " for parameter ", pn))
        })
      }  

      is.grid = FALSE
      #restore.point("test")
      #if (pn=="q")
      #  stop()
      
      if (is.data.frame(p))
        p = as.matrix(p)
   
      if (is.na(mi$param.sets[[pn]])[1]) {
         if (length(p)>1) {
          stop(paste("Error: Model file specifies parameter",pn," as a single number, but you provided a vector: ",pn, "=",paste(p,collapse=" ")))
        }
      } else if (length(pset)==1) {
        if (length(p) != length(pset[[1]])) {
          stop(paste("Error: Parameter ",pn," is defined over set ", paste(names(pset),collapse=" "), " and should have ", length(pset[[1]]), " elements, but the argument has ", length(p), "elements"))
        }
      } else if (length(pset)==2) {
        if (!(is.matrix(p) | is.data.frame(p))) {
          stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you did not provide a matrix."))          
        }
        if (NCOL(p)==3 & NROW(p) == length(pset[[1]]) * length(pset[[2]])) {
          is.grid=TRUE
        } else {  
          if (NROW(p)!=length(pset[[1]]) | NCOL(p)!=length(pset[[2]])) {
            stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you provided a ",NROW(p), "x", NCOL(p), " matrix."))          
          }
        }
      } else if (length(pset)>=3) {
        if (NCOL(p)!=length(pset)+1) {
          stop(paste("Error: Parameter ",pn," is defined over the sets: ", paste(names(pset),collapse=","), ". You must provide a grid with ", length(pset)," index columns and one value column."))
        }
        is.grid = TRUE
      }

      # A matrix
      if (is.matrix(p) & (!is.grid)) {
        tstr = apply(p,1,function (row) paste(row,collapse=" "))
        rowset = pset[[1]]
        colset = pset[[2]]
        tstr = paste("      ",rowset,"  ",tstr,collapse="\n",sep="")
        tstr = mypaste(" param ",pn,": ",paste(colset,collapse=" "),":= \n",
                                         tstr, ";\n")
        
        str[i] = tstr  
      # A parameter defined over a single set
      } else if (is.grid) {
        id.grid = p[,-NCOL(p)]
        id.txt = paste.matrix.cols(id.grid,sep=",")
        row.txt = paste("[",id.txt,"]",p[,NCOL(p)])
        
        str[i] = mypaste("  param ",pn, " := ", paste(row.txt,collapse=" "),";\n")        
        
      } else if (!is.na(mi$param.sets[[pn]])[1]) {        
        str[i] = mypaste("  param ",pn, " := ", mypaste("[",pset[[1]],"] ",p),";\n")        
      # A single number
      } else {
        str[i] = mypaste("  param ",pn, " := ", p,";\n")
      }
    }
    str = mypaste(
"
/* Parameter */
",
    mypaste(str,collapse="\n"))
  }
  txt = paste(txt,str,sep="\n")

  if (!is.null(dat.file)) {
    writeLines(txt,dat.file)
  } else {
    return(txt)
  }
}




#' Load a GMPL model and data and generate a GLPK object
#' @export
gmpl.load.problem = function(mod.file,dat.file) {
  require(glpkAPI)
  wk  = mplAllocWkspGLPK()
  mplReadModelGLPK(wk,mod.file,skip=0)
  mplReadDataGLPK(wk,dat.file)
  mplGenerateGLPK(wk, fname = NULL)
  
  lp <- initProbGLPK()
  mplBuildProbGLPK(wk, lp)
  mplFreeWkspGLPK(wk)
  lp
}

#' Solve a GMPL problem using glpkAPI
#' @param mod.file path of the .mod file in which the gmpl model is specified
#' @param dat.file path of the .dat file in which the gmpl data is specified. If NULL generate a new .dat file from the given sets and param with the same name as the model file
#' @param sets a list with the sets used by the gmpl model. Needed if no dat.file specified
#' @param param a list with the parameters used by the gmpl model. Needed if no dat.file specified
#' @param lp optional a link to the GLPK problem generated by gmpl.load.problem
#' @param delete.lp default = TRUE if lp is given, shall it be removed from memory after it has been solved?
#' @param adapt.sol default = TRUE shall the solution be returned in a more convenient form
#' @param solver The solver "Simplex" (default), "MIP", "Interior" or "SimplexExact"
#' @export 
gmpl.solve = function(mod.file=NULL,dat.file=NULL,sets=NULL, param=NULL,lp=NULL,delete.lp = is.null(lp),adapt.sol=TRUE, solver=c("Simplex","MIP","Interior","SimplexExact")[if (isTRUE(model.info$is.mip)) 2 else 1], model.info=NULL) {
  # Generate a dat.file
  if (is.null(dat.file)) {
    dat.file = paste(substring(mod.file,1,nchar(mod.file)-4),".dat",sep="")
    message("Generate dat.file: ", dat.file)
    gmpl.make.dat.file(sets=sets,param=param,mod.file=mod.file,dat.file=dat.file)
  }
  
  
  if (is.null(lp)) {
    message("Load problem...")
    lp = gmpl.load.problem(mod.file,dat.file)
  }
  message("Solve problem...")

  res = glpk.solve(lp,delete.lp, solver=solver)
  
  if (adapt.sol) {
    res$sol = gmpl.adapt.sol(res$sol,mod.file=mod.file)
  }
  return(res)
}


examples.gmpl.solve = examples.gmpl.make.dat.file = function() {
  
  # Model of power plant investments and dispatch included in package
  mod.file = paste(.path.package(package = "rgmpl"),"/data/power.mod",sep="")
    
  # Name of dat file, will be generated locally
  dat.file = "power.dat"
  
  # Example data
  
  # Sets
  PLANTS = c("coal","gas")
  PERIODS = 1:4
  sets = list(PLANTS=PLANTS,PERIODS=PERIODS)
  
  # Parameters
  fc = c(12,6)  # fixed cost 
  vc = c(18,30) # variable cost
  load = c(30,50,25,20) # electricity demand
  T = length(PERIODS)
  param = list(vc=vc,fc=fc,load=load,T=T)
  
  # Generate a GMPL .dat file
  gmpl.make.dat.file(sets=sets,param=param,mod.file=mod.file, dat.file=dat.file)
  
  # Solve the model
  res = gmpl.solve(mod.file=mod.file,dat.file=dat.file, delete.lp =FALSE)
  res
  
  # Show production levels graphically
  library(ggplot2)
  qplot(data=res$sol$q, x=PERIODS,y=q,fill=PLANTS,geom="bar",stats="identity", xlab="Period",ylab="Production")
  
}
#' Solve a GLPK linear problem
#' 
#' @param lp a GLPK problem generated e.g. by a call to gmpl.load.problem
#' @param delete.lp default = TRUE shall the problem lp be removed from memory after it has been solved?
#' @param solver The solver "Simplex" (default), "MIP", "Interior" or "SimplexExact"
#' @export
glpk.solve = function(lp=NULL, delete.lp = TRUE, solver=c("Simplex","MIP","Interior","SimplexExact")[1]) {
  # solve model with simplex algorithm
  #require(glpkAPI)
  restore.point("glpk.solve")
  
  message("Solve model...")

  if (solver=="MIP") {
    # pre solve as simplex
    code = solveSimplexGLPK(lp)
  }
  fun = paste0("solve",solver,"GLPK")
  code = do.call(fun, list(lp))
  message("Retrieve solution")
  # Retrieve Solution
  nc = getNumColsGLPK(lp)
  nr = getNumRowsGLPK(lp)
  
  #nc = glp_get_num_cols(lp)
  #nr = glp_get_num_rows(lp)
  
  #val = glp_get_obj_val(lp)
  val = getObjValGLPK(lp)
  #sol           = mapply(getColPrimGLPK,j=1:nc,MoreArgs=list(lp=lp))
  if (solver == "MIP") {
    sol = mipColsValGLPK(lp)
  } else {
    sol = getColsPrimGLPK(lp)
  }
  sol.name      = mapply(getColNameGLPK,j=1:nc,MoreArgs=list(lp=lp))
  
  names(sol)    = sol.name
  
  duals         = mapply(getRowDualGLPK,i=1:nr,MoreArgs=list(lp=lp))
  constr.name   = mapply(getRowNameGLPK,i=1:nr,MoreArgs=list(lp=lp))
  names(duals) = constr.name
  
  # Delete the linear program in order to free memory
  if (delete.lp) {
    delProbGLPK(lp)
    #glp_delete_prob(lp)
  }
  
  return(list(code=code,val=val,sol=sol,duals=duals))
}

# Internal function write solution in a nicer form
gmpl.adapt.sol = function(sol,mod.file=NULL,mi=NULL) {
  #restore.point("adapt.sol")
  
  lab = names(sol)
  left.bracket.pos = str_locate(lab,fixed("["))[,1]-1
  na.rows = is.na(left.bracket.pos)
  left.bracket.pos[is.na(left.bracket.pos)] = nchar(lab[na.rows])
  var = str_sub(lab,start=1,end=left.bracket.pos)
  arg.str = str_sub(lab,start=left.bracket.pos+2,end=-2)
  
  if (!is.null(mod.file)) {
    mi = gmpl.get.model.info(mod.file)
  }
  
  ret = list()
  for (vn in unique(var)) {
    rows = which(var == vn)
    # A scalar variable
    if (nchar(arg.str[rows[1]]) == 0) {
      ret[[vn]] = as.numeric(sol[vn])
      next
    }  
    num.comma = NROW(str_locate_all(arg.str[rows[1]],fixed(","))[[1]])
  
    
    df = as.data.frame(matrix(NA,NROW(rows),num.comma+2))
    act.arg.str = arg.str[rows]
    if (num.comma>0) {
      for (i in 1:num.comma) {
        comma.pos = str_locate(act.arg.str,fixed(","))[,1]
        df[,i] =  str_sub(act.arg.str,1,comma.pos-1)
        act.arg.str = str_sub(act.arg.str,start=comma.pos+1,end=-1)
      }
    }
    df[,num.comma+1] = act.arg.str
    df[,num.comma+2] = sol[rows]
    if (!is.null(mi)) {
      colnames(df)=c(mi$var.sets[[vn]],vn)
    } else {
      colnames(df)=c(paste("arg",1:(num.comma+1),sep=""),vn)
    }
    
    if (NROW(df)>1 & NCOL(df)>1) {
      for (i in 1:NCOL(df)) {
        if (is.character(df[1,i]) & !is.na(suppressWarnings(as.numeric(df[1,i]))) ) {
          num.col= suppressWarnings(as.numeric(df[,i]))
          if (sum(is.na(num.col))==0)
            df[,i] = num.col
        }        
      }
    }
    
    ret[[vn]]=df
  }
  return(ret)
} 


#' Paste together columns of a matrix or data.frame
paste.matrix.cols = function(mat,cols=1:NCOL(mat),...) {
  if (NROW(cols)==2) {
    return(paste(mat[,cols[1]],mat[,cols[2]],...))
  } else if (NROW(cols)==3) {
    return(paste(mat[,cols[1]],mat[,cols[2]],mat[,cols[3]],...))
  } else {
    code = paste("mat[,",cols,"]",collapse=",")
    code = paste("paste(",code,",...)",sep="")
    return(eval(parse(text=code)))
  }
}
