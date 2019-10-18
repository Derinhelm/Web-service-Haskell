-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.1-beta
-- PostgreSQL version: 10.0
-- Project Site: pgmodeler.com.br
-- Model Author: ---


-- Database creation must be done outside an multicommand file.
-- These commands were put in this file only for convenience.
-- -- object: graph | type: DATABASE --
-- -- DROP DATABASE IF EXISTS graph;
-- CREATE DATABASE graph
-- ;
-- -- ddl-end --
-- 

-- object: public.node | type: TABLE --
-- DROP TABLE IF EXISTS public.node CASCADE;
CREATE TABLE public.node(
	id_node serial NOT NULL,
	label varchar(20),
	CONSTRAINT node_pk PRIMARY KEY (id_node)

);
-- ddl-end --
ALTER TABLE public.node OWNER TO postgres;
-- ddl-end --

-- object: public.edge | type: TABLE --
-- DROP TABLE IF EXISTS public.edge CASCADE;
CREATE TABLE public.edge(
	id_edge serial NOT NULL,
	first_node integer,
	second_node integer,
	CONSTRAINT edge_pk PRIMARY KEY (id_edge)

);
-- ddl-end --
ALTER TABLE public.edge OWNER TO postgres;
-- ddl-end --

-- object: f_1 | type: CONSTRAINT --
-- ALTER TABLE public.edge DROP CONSTRAINT IF EXISTS f_1 CASCADE;
ALTER TABLE public.edge ADD CONSTRAINT f_1 FOREIGN KEY (first_node)
REFERENCES public.node (id_node) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: f_2 | type: CONSTRAINT --
-- ALTER TABLE public.edge DROP CONSTRAINT IF EXISTS f_2 CASCADE;
ALTER TABLE public.edge ADD CONSTRAINT f_2 FOREIGN KEY (second_node)
REFERENCES public.node (id_node) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --


